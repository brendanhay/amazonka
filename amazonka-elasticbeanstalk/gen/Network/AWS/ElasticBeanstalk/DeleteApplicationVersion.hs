{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified version from the specified application. You cannot
-- delete an application version that is associated with a running
-- environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=First%20Release &Operation=DeleteApplicationVersion
-- &AuthParams 58dc7339-f272-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    (
    -- * Request
      DeleteApplicationVersion
    -- ** Request constructor
    , deleteApplicationVersion
    -- ** Request lenses
    , davApplicationName
    , davVersionLabel
    , davDeleteSourceBundle

    -- * Response
    , DeleteApplicationVersionResponse
    -- ** Response constructor
    , deleteApplicationVersionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DeleteApplicationVersion = DeleteApplicationVersion
    { _davApplicationName :: Text
    , _davVersionLabel :: Text
    , _davDeleteSourceBundle :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteApplicationVersion' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Text@
--
-- * @VersionLabel ::@ @Text@
--
-- * @DeleteSourceBundle ::@ @Maybe Bool@
--
deleteApplicationVersion :: Text -- ^ 'davApplicationName'
                         -> Text -- ^ 'davVersionLabel'
                         -> DeleteApplicationVersion
deleteApplicationVersion p1 p2 = DeleteApplicationVersion
    { _davApplicationName = p1
    , _davVersionLabel = p2
    , _davDeleteSourceBundle = Nothing
    }

-- | The name of the application to delete releases from.
davApplicationName :: Lens' DeleteApplicationVersion Text
davApplicationName =
    lens _davApplicationName (\s a -> s { _davApplicationName = a })

-- | The label of the version to delete.
davVersionLabel :: Lens' DeleteApplicationVersion Text
davVersionLabel = lens _davVersionLabel (\s a -> s { _davVersionLabel = a })

-- | Indicates whether to delete the associated source bundle from Amazon S3:
-- true: An attempt is made to delete the associated Amazon S3 source bundle
-- specified at time of creation. false: No action is taken on the Amazon S3
-- source bundle specified at time of creation. Valid Values: true | false.
davDeleteSourceBundle :: Lens' DeleteApplicationVersion (Maybe Bool)
davDeleteSourceBundle =
    lens _davDeleteSourceBundle (\s a -> s { _davDeleteSourceBundle = a })

instance ToQuery DeleteApplicationVersion where
    toQuery = genericQuery def

data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteApplicationVersionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteApplicationVersionResponse :: DeleteApplicationVersionResponse
deleteApplicationVersionResponse = DeleteApplicationVersionResponse

instance AWSRequest DeleteApplicationVersion where
    type Sv DeleteApplicationVersion = ElasticBeanstalk
    type Rs DeleteApplicationVersion = DeleteApplicationVersionResponse

    request = post "DeleteApplicationVersion"
    response _ = nullaryResponse DeleteApplicationVersionResponse
