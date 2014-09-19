{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the password for the specified user, which terminates the user's
-- ability to access AWS services through the AWS Management Console. Deleting
-- a user's password does not prevent a user from accessing IAM through the
-- command line interface or the API. To prevent all user access you must also
-- either make the access key inactive or delete it. For more information
-- about making keys inactive or deleting them, see UpdateAccessKey and
-- DeleteAccessKey. https://iam.amazonaws.com/ ?Action=DeleteLoginProfile
-- &UserName=Bob &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteLoginProfile
    (
    -- * Request
      DeleteLoginProfile
    -- ** Request constructor
    , deleteLoginProfile
    -- ** Request lenses
    , dlpUserName

    -- * Response
    , DeleteLoginProfileResponse
    -- ** Response constructor
    , deleteLoginProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteLoginProfile = DeleteLoginProfile
    { _dlpUserName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoginProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
deleteLoginProfile :: Text -- ^ 'dlpUserName'
                   -> DeleteLoginProfile
deleteLoginProfile p1 = DeleteLoginProfile
    { _dlpUserName = p1
    }

-- | Name of the user whose password you want to delete.
dlpUserName :: Lens' DeleteLoginProfile Text
dlpUserName = lens _dlpUserName (\s a -> s { _dlpUserName = a })

instance ToQuery DeleteLoginProfile where
    toQuery = genericQuery def

data DeleteLoginProfileResponse = DeleteLoginProfileResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteLoginProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteLoginProfileResponse :: DeleteLoginProfileResponse
deleteLoginProfileResponse = DeleteLoginProfileResponse

instance AWSRequest DeleteLoginProfile where
    type Sv DeleteLoginProfile = IAM
    type Rs DeleteLoginProfile = DeleteLoginProfileResponse

    request = post "DeleteLoginProfile"
    response _ = nullaryResponse DeleteLoginProfileResponse
