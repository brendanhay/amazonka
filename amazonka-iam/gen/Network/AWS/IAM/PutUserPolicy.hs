{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified user. For
-- information about policies, refer to Overview of Policies in the Using IAM
-- guide. For information about limits on the number of policies you can
-- associate with a user, see Limitations on IAM Entities in the Using IAM
-- guide.
module Network.AWS.IAM.PutUserPolicy
    (
    -- * Request
      PutUserPolicy
    -- ** Request constructor
    , putUserPolicy
    -- ** Request lenses
    , pupPolicyDocument
    , pupPolicyName
    , pupUserName

    -- * Response
    , PutUserPolicyResponse
    -- ** Response constructor
    , putUserPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data PutUserPolicy = PutUserPolicy
    { _pupPolicyDocument :: Text
    , _pupPolicyName     :: Text
    , _pupUserName       :: Text
    } (Eq, Ord, Show, Generic)

-- | 'PutUserPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pupPolicyDocument' @::@ 'Text'
--
-- * 'pupPolicyName' @::@ 'Text'
--
-- * 'pupUserName' @::@ 'Text'
--
putUserPolicy :: Text -- ^ 'pupUserName'
              -> Text -- ^ 'pupPolicyName'
              -> Text -- ^ 'pupPolicyDocument'
              -> PutUserPolicy
putUserPolicy p1 p2 p3 = PutUserPolicy
    { _pupUserName       = p1
    , _pupPolicyName     = p2
    , _pupPolicyDocument = p3
    }

-- | The policy document.
pupPolicyDocument :: Lens' PutUserPolicy Text
pupPolicyDocument =
    lens _pupPolicyDocument (\s a -> s { _pupPolicyDocument = a })

-- | The name of the policy document.
pupPolicyName :: Lens' PutUserPolicy Text
pupPolicyName = lens _pupPolicyName (\s a -> s { _pupPolicyName = a })

-- | The name of the user to associate the policy with.
pupUserName :: Lens' PutUserPolicy Text
pupUserName = lens _pupUserName (\s a -> s { _pupUserName = a })
instance ToQuery PutUserPolicy

instance ToPath PutUserPolicy where
    toPath = const "/"

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutUserPolicyResponse' constructor.
putUserPolicyResponse :: PutUserPolicyResponse
putUserPolicyResponse = PutUserPolicyResponse

instance FromXML PutUserPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutUserPolicyResponse"

instance AWSRequest PutUserPolicy where
    type Sv PutUserPolicy = IAM
    type Rs PutUserPolicy = PutUserPolicyResponse

    request  = post "PutUserPolicy"
    response = nullaryResponse PutUserPolicyResponse
