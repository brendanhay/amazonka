{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkACLEntry
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ingress or egress entry (rule) from the specified
-- network ACL.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkACLEntry.html>
module Network.AWS.EC2.DeleteNetworkACLEntry
    (
    -- * Request
      DeleteNetworkACLEntry
    -- ** Request constructor
    , deleteNetworkACLEntry
    -- ** Request lenses
    , dnaerqDryRun
    , dnaerqNetworkACLId
    , dnaerqRuleNumber
    , dnaerqEgress

    -- * Response
    , DeleteNetworkACLEntryResponse
    -- ** Response constructor
    , deleteNetworkACLEntryResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteNetworkACLEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnaerqDryRun'
--
-- * 'dnaerqNetworkACLId'
--
-- * 'dnaerqRuleNumber'
--
-- * 'dnaerqEgress'
data DeleteNetworkACLEntry = DeleteNetworkACLEntry'
    { _dnaerqDryRun       :: !(Maybe Bool)
    , _dnaerqNetworkACLId :: !Text
    , _dnaerqRuleNumber   :: !Int
    , _dnaerqEgress       :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteNetworkACLEntry' smart constructor.
deleteNetworkACLEntry :: Text -> Int -> Bool -> DeleteNetworkACLEntry
deleteNetworkACLEntry pNetworkACLId pRuleNumber pEgress =
    DeleteNetworkACLEntry'
    { _dnaerqDryRun = Nothing
    , _dnaerqNetworkACLId = pNetworkACLId
    , _dnaerqRuleNumber = pRuleNumber
    , _dnaerqEgress = pEgress
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dnaerqDryRun :: Lens' DeleteNetworkACLEntry (Maybe Bool)
dnaerqDryRun = lens _dnaerqDryRun (\ s a -> s{_dnaerqDryRun = a});

-- | The ID of the network ACL.
dnaerqNetworkACLId :: Lens' DeleteNetworkACLEntry Text
dnaerqNetworkACLId = lens _dnaerqNetworkACLId (\ s a -> s{_dnaerqNetworkACLId = a});

-- | The rule number of the entry to delete.
dnaerqRuleNumber :: Lens' DeleteNetworkACLEntry Int
dnaerqRuleNumber = lens _dnaerqRuleNumber (\ s a -> s{_dnaerqRuleNumber = a});

-- | Indicates whether the rule is an egress rule.
dnaerqEgress :: Lens' DeleteNetworkACLEntry Bool
dnaerqEgress = lens _dnaerqEgress (\ s a -> s{_dnaerqEgress = a});

instance AWSRequest DeleteNetworkACLEntry where
        type Sv DeleteNetworkACLEntry = EC2
        type Rs DeleteNetworkACLEntry =
             DeleteNetworkACLEntryResponse
        request = post
        response = receiveNull DeleteNetworkACLEntryResponse'

instance ToHeaders DeleteNetworkACLEntry where
        toHeaders = const mempty

instance ToPath DeleteNetworkACLEntry where
        toPath = const "/"

instance ToQuery DeleteNetworkACLEntry where
        toQuery DeleteNetworkACLEntry'{..}
          = mconcat
              ["Action" =: ("DeleteNetworkACLEntry" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dnaerqDryRun,
               "NetworkAclId" =: _dnaerqNetworkACLId,
               "RuleNumber" =: _dnaerqRuleNumber,
               "Egress" =: _dnaerqEgress]

-- | /See:/ 'deleteNetworkACLEntryResponse' smart constructor.
data DeleteNetworkACLEntryResponse =
    DeleteNetworkACLEntryResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteNetworkACLEntryResponse' smart constructor.
deleteNetworkACLEntryResponse :: DeleteNetworkACLEntryResponse
deleteNetworkACLEntryResponse = DeleteNetworkACLEntryResponse'
