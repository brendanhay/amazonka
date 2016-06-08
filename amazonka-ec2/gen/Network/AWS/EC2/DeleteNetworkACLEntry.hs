{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkACLEntry
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified ingress or egress entry (rule) from the specified network ACL.
module Network.AWS.EC2.DeleteNetworkACLEntry
    (
    -- * Creating a Request
      deleteNetworkACLEntry
    , DeleteNetworkACLEntry
    -- * Request Lenses
    , dnaeDryRun
    , dnaeNetworkACLId
    , dnaeRuleNumber
    , dnaeEgress

    -- * Destructuring the Response
    , deleteNetworkACLEntryResponse
    , DeleteNetworkACLEntryResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DeleteNetworkAclEntry.
--
-- /See:/ 'deleteNetworkACLEntry' smart constructor.
data DeleteNetworkACLEntry = DeleteNetworkACLEntry'
    { _dnaeDryRun       :: !(Maybe Bool)
    , _dnaeNetworkACLId :: !Text
    , _dnaeRuleNumber   :: !Int
    , _dnaeEgress       :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteNetworkACLEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnaeDryRun'
--
-- * 'dnaeNetworkACLId'
--
-- * 'dnaeRuleNumber'
--
-- * 'dnaeEgress'
deleteNetworkACLEntry
    :: Text -- ^ 'dnaeNetworkACLId'
    -> Int -- ^ 'dnaeRuleNumber'
    -> Bool -- ^ 'dnaeEgress'
    -> DeleteNetworkACLEntry
deleteNetworkACLEntry pNetworkACLId_ pRuleNumber_ pEgress_ =
    DeleteNetworkACLEntry'
    { _dnaeDryRun = Nothing
    , _dnaeNetworkACLId = pNetworkACLId_
    , _dnaeRuleNumber = pRuleNumber_
    , _dnaeEgress = pEgress_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dnaeDryRun :: Lens' DeleteNetworkACLEntry (Maybe Bool)
dnaeDryRun = lens _dnaeDryRun (\ s a -> s{_dnaeDryRun = a});

-- | The ID of the network ACL.
dnaeNetworkACLId :: Lens' DeleteNetworkACLEntry Text
dnaeNetworkACLId = lens _dnaeNetworkACLId (\ s a -> s{_dnaeNetworkACLId = a});

-- | The rule number of the entry to delete.
dnaeRuleNumber :: Lens' DeleteNetworkACLEntry Int
dnaeRuleNumber = lens _dnaeRuleNumber (\ s a -> s{_dnaeRuleNumber = a});

-- | Indicates whether the rule is an egress rule.
dnaeEgress :: Lens' DeleteNetworkACLEntry Bool
dnaeEgress = lens _dnaeEgress (\ s a -> s{_dnaeEgress = a});

instance AWSRequest DeleteNetworkACLEntry where
        type Rs DeleteNetworkACLEntry =
             DeleteNetworkACLEntryResponse
        request = postQuery ec2
        response = receiveNull DeleteNetworkACLEntryResponse'

instance Hashable DeleteNetworkACLEntry

instance NFData DeleteNetworkACLEntry

instance ToHeaders DeleteNetworkACLEntry where
        toHeaders = const mempty

instance ToPath DeleteNetworkACLEntry where
        toPath = const "/"

instance ToQuery DeleteNetworkACLEntry where
        toQuery DeleteNetworkACLEntry'{..}
          = mconcat
              ["Action" =: ("DeleteNetworkAclEntry" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "DryRun" =: _dnaeDryRun,
               "NetworkAclId" =: _dnaeNetworkACLId,
               "RuleNumber" =: _dnaeRuleNumber,
               "Egress" =: _dnaeEgress]

-- | /See:/ 'deleteNetworkACLEntryResponse' smart constructor.
data DeleteNetworkACLEntryResponse =
    DeleteNetworkACLEntryResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteNetworkACLEntryResponse' with the minimum fields required to make a request.
--
deleteNetworkACLEntryResponse
    :: DeleteNetworkACLEntryResponse
deleteNetworkACLEntryResponse = DeleteNetworkACLEntryResponse'

instance NFData DeleteNetworkACLEntryResponse
