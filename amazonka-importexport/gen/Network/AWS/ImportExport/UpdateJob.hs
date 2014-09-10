{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | You use this operation to change the parameters specified in the original
-- manifest file by supplying a new manifest file. The manifest file attached
-- to this request replaces the original manifest file. You can only use the
-- operation after a CreateJob request but before the data transfer starts and
-- you can only use it on jobs you own.
module Network.AWS.ImportExport
    (
    -- * Request
      UpdateJob
    -- ** Request constructor
    , mkUpdateJob
    -- ** Request lenses
    , ujJobId
    , ujManifest
    , ujJobType
    , ujValidateOnly

    -- * Response
    , UpdateJobResponse
    -- ** Response constructor
    , mkUpdateJobResponse
    -- ** Response lenses
    , ujrSuccess
    , ujrWarningMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import Network.AWS.Prelude

-- | Input structure for the UpateJob operation.
data UpdateJob = UpdateJob
    { _ujJobId :: !Text
    , _ujManifest :: !Text
    , _ujJobType :: JobType
    , _ujValidateOnly :: !Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateJob' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @JobId ::@ @Text@
--
-- * @Manifest ::@ @Text@
--
-- * @JobType ::@ @JobType@
--
-- * @ValidateOnly ::@ @Bool@
--
mkUpdateJob :: Text -- ^ 'ujJobId'
            -> Text -- ^ 'ujManifest'
            -> JobType -- ^ 'ujJobType'
            -> Bool -- ^ 'ujValidateOnly'
            -> UpdateJob
mkUpdateJob p1 p2 p3 p4 = UpdateJob
    { _ujJobId = p1
    , _ujManifest = p2
    , _ujJobType = p3
    , _ujValidateOnly = p4
    }

-- | A unique identifier which refers to a particular job.
ujJobId :: Lens' UpdateJob Text
ujJobId = lens _ujJobId (\s a -> s { _ujJobId = a })

-- | The UTF-8 encoded text of the manifest file.
ujManifest :: Lens' UpdateJob Text
ujManifest = lens _ujManifest (\s a -> s { _ujManifest = a })

-- | Specifies whether the job to initiate is an import or export job.
ujJobType :: Lens' UpdateJob JobType
ujJobType = lens _ujJobType (\s a -> s { _ujJobType = a })

-- | Validate the manifest and parameter values in the request but do not
-- actually create a job.
ujValidateOnly :: Lens' UpdateJob Bool
ujValidateOnly = lens _ujValidateOnly (\s a -> s { _ujValidateOnly = a })

instance ToQuery UpdateJob where
    toQuery = genericQuery def

-- | Output structure for the UpateJob operation.
data UpdateJobResponse = UpdateJobResponse
    { _ujrSuccess :: !(Maybe Bool)
    , _ujrWarningMessage :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateJobResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Success ::@ @Maybe Bool@
--
-- * @WarningMessage ::@ @Maybe Text@
--
mkUpdateJobResponse :: UpdateJobResponse
mkUpdateJobResponse = UpdateJobResponse
    { _ujrSuccess = Nothing
    , _ujrWarningMessage = Nothing
    }

-- | Specifies whether (true) or not (false) AWS Import/Export updated your job.
ujrSuccess :: Lens' UpdateJobResponse (Maybe Bool)
ujrSuccess = lens _ujrSuccess (\s a -> s { _ujrSuccess = a })

-- | An optional message notifying you of non-fatal issues with the job, such as
-- use of an incompatible Amazon S3 bucket name.
ujrWarningMessage :: Lens' UpdateJobResponse (Maybe Text)
ujrWarningMessage =
    lens _ujrWarningMessage (\s a -> s { _ujrWarningMessage = a })

instance FromXML UpdateJobResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateJob where
    type Sv UpdateJob = ImportExport
    type Rs UpdateJob = UpdateJobResponse

    request = post "UpdateJob"
    response _ = xmlResponse
