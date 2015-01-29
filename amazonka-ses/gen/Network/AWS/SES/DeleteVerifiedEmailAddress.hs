{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified email address from the list of verified addresses.
--
-- The DeleteVerifiedEmailAddress action is deprecated as of the May 15, 2012
-- release of Domain Verification. The DeleteIdentity action is now preferred. This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_DeleteVerifiedEmailAddress.html>
module Network.AWS.SES.DeleteVerifiedEmailAddress
    (
    -- * Request
      DeleteVerifiedEmailAddress
    -- ** Request constructor
    , deleteVerifiedEmailAddress
    -- ** Request lenses
    , dveaEmailAddress

    -- * Response
    , DeleteVerifiedEmailAddressResponse
    -- ** Response constructor
    , deleteVerifiedEmailAddressResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types
import qualified GHC.Exts

newtype DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress
    { _dveaEmailAddress :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteVerifiedEmailAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dveaEmailAddress' @::@ 'Text'
--
deleteVerifiedEmailAddress :: Text -- ^ 'dveaEmailAddress'
                           -> DeleteVerifiedEmailAddress
deleteVerifiedEmailAddress p1 = DeleteVerifiedEmailAddress
    { _dveaEmailAddress = p1
    }

-- | An email address to be removed from the list of verified addresses.
dveaEmailAddress :: Lens' DeleteVerifiedEmailAddress Text
dveaEmailAddress = lens _dveaEmailAddress (\s a -> s { _dveaEmailAddress = a })

data DeleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteVerifiedEmailAddressResponse' constructor.
deleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse
deleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse

instance ToPath DeleteVerifiedEmailAddress where
    toPath = const "/"

instance ToQuery DeleteVerifiedEmailAddress where
    toQuery DeleteVerifiedEmailAddress{..} = mconcat
        [ "EmailAddress" =? _dveaEmailAddress
        ]

instance ToHeaders DeleteVerifiedEmailAddress

instance AWSRequest DeleteVerifiedEmailAddress where
    type Sv DeleteVerifiedEmailAddress = SES
    type Rs DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddressResponse

    request  = post "DeleteVerifiedEmailAddress"
    response = nullResponse DeleteVerifiedEmailAddressResponse
