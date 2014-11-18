{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAclEntry.html>
module Network.AWS.EC2.DeleteNetworkAclEntry
    (
    -- * Request
      DeleteNetworkAclEntry
    -- ** Request constructor
    , deleteNetworkAclEntry
    -- ** Request lenses
    , dnaeDryRun
    , dnaeEgress
    , dnaeNetworkAclId
    , dnaeRuleNumber

    -- * Response
    , DeleteNetworkAclEntryResponse
    -- ** Response constructor
    , deleteNetworkAclEntryResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { _dnaeDryRun       :: Maybe Bool
    , _dnaeEgress       :: Bool
    , _dnaeNetworkAclId :: Text
    , _dnaeRuleNumber   :: Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNetworkAclEntry' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnaeDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dnaeEgress' @::@ 'Bool'
--
-- * 'dnaeNetworkAclId' @::@ 'Text'
--
-- * 'dnaeRuleNumber' @::@ 'Int'
--
deleteNetworkAclEntry :: Text -- ^ 'dnaeNetworkAclId'
                      -> Int -- ^ 'dnaeRuleNumber'
                      -> Bool -- ^ 'dnaeEgress'
                      -> DeleteNetworkAclEntry
deleteNetworkAclEntry p1 p2 p3 = DeleteNetworkAclEntry
    { _dnaeNetworkAclId = p1
    , _dnaeRuleNumber   = p2
    , _dnaeEgress       = p3
    , _dnaeDryRun       = Nothing
    }

dnaeDryRun :: Lens' DeleteNetworkAclEntry (Maybe Bool)
dnaeDryRun = lens _dnaeDryRun (\s a -> s { _dnaeDryRun = a })

-- | Indicates whether the rule is an egress rule.
dnaeEgress :: Lens' DeleteNetworkAclEntry Bool
dnaeEgress = lens _dnaeEgress (\s a -> s { _dnaeEgress = a })

-- | The ID of the network ACL.
dnaeNetworkAclId :: Lens' DeleteNetworkAclEntry Text
dnaeNetworkAclId = lens _dnaeNetworkAclId (\s a -> s { _dnaeNetworkAclId = a })

-- | The rule number of the entry to delete.
dnaeRuleNumber :: Lens' DeleteNetworkAclEntry Int
dnaeRuleNumber = lens _dnaeRuleNumber (\s a -> s { _dnaeRuleNumber = a })

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNetworkAclEntryResponse' constructor.
deleteNetworkAclEntryResponse :: DeleteNetworkAclEntryResponse
deleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse

instance ToPath DeleteNetworkAclEntry where
    toPath = const "/"

instance ToQuery DeleteNetworkAclEntry

instance ToHeaders DeleteNetworkAclEntry

instance AWSRequest DeleteNetworkAclEntry where
    type Sv DeleteNetworkAclEntry = EC2
    type Rs DeleteNetworkAclEntry = DeleteNetworkAclEntryResponse

    request  = post "DeleteNetworkAclEntry"
    response = nullResponse DeleteNetworkAclEntryResponse
