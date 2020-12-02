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
-- Module      : Network.AWS.Redshift.ModifyClusterIAMRoles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the list of AWS Identity and Access Management (IAM) roles that can be used by the cluster to access other AWS services.
--
--
-- A cluster can have up to 10 IAM roles associated at any time.
--
module Network.AWS.Redshift.ModifyClusterIAMRoles
    (
    -- * Creating a Request
      modifyClusterIAMRoles
    , ModifyClusterIAMRoles
    -- * Request Lenses
    , mcirRemoveIAMRoles
    , mcirAddIAMRoles
    , mcirClusterIdentifier

    -- * Destructuring the Response
    , modifyClusterIAMRolesResponse
    , ModifyClusterIAMRolesResponse
    -- * Response Lenses
    , mcirrsCluster
    , mcirrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyClusterIAMRoles' smart constructor.
data ModifyClusterIAMRoles = ModifyClusterIAMRoles'
  { _mcirRemoveIAMRoles    :: !(Maybe [Text])
  , _mcirAddIAMRoles       :: !(Maybe [Text])
  , _mcirClusterIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterIAMRoles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcirRemoveIAMRoles' - Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
--
-- * 'mcirAddIAMRoles' - Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
--
-- * 'mcirClusterIdentifier' - The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
modifyClusterIAMRoles
    :: Text -- ^ 'mcirClusterIdentifier'
    -> ModifyClusterIAMRoles
modifyClusterIAMRoles pClusterIdentifier_ =
  ModifyClusterIAMRoles'
    { _mcirRemoveIAMRoles = Nothing
    , _mcirAddIAMRoles = Nothing
    , _mcirClusterIdentifier = pClusterIdentifier_
    }


-- | Zero or more IAM roles in ARN format to disassociate from the cluster. You can disassociate up to 10 IAM roles from a single cluster in a single request.
mcirRemoveIAMRoles :: Lens' ModifyClusterIAMRoles [Text]
mcirRemoveIAMRoles = lens _mcirRemoveIAMRoles (\ s a -> s{_mcirRemoveIAMRoles = a}) . _Default . _Coerce

-- | Zero or more IAM roles to associate with the cluster. The roles must be in their Amazon Resource Name (ARN) format. You can associate up to 10 IAM roles with a single cluster in a single request.
mcirAddIAMRoles :: Lens' ModifyClusterIAMRoles [Text]
mcirAddIAMRoles = lens _mcirAddIAMRoles (\ s a -> s{_mcirAddIAMRoles = a}) . _Default . _Coerce

-- | The unique identifier of the cluster for which you want to associate or disassociate IAM roles.
mcirClusterIdentifier :: Lens' ModifyClusterIAMRoles Text
mcirClusterIdentifier = lens _mcirClusterIdentifier (\ s a -> s{_mcirClusterIdentifier = a})

instance AWSRequest ModifyClusterIAMRoles where
        type Rs ModifyClusterIAMRoles =
             ModifyClusterIAMRolesResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "ModifyClusterIamRolesResult"
              (\ s h x ->
                 ModifyClusterIAMRolesResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable ModifyClusterIAMRoles where

instance NFData ModifyClusterIAMRoles where

instance ToHeaders ModifyClusterIAMRoles where
        toHeaders = const mempty

instance ToPath ModifyClusterIAMRoles where
        toPath = const "/"

instance ToQuery ModifyClusterIAMRoles where
        toQuery ModifyClusterIAMRoles'{..}
          = mconcat
              ["Action" =: ("ModifyClusterIamRoles" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "RemoveIamRoles" =:
                 toQuery
                   (toQueryList "IamRoleArn" <$> _mcirRemoveIAMRoles),
               "AddIamRoles" =:
                 toQuery
                   (toQueryList "IamRoleArn" <$> _mcirAddIAMRoles),
               "ClusterIdentifier" =: _mcirClusterIdentifier]

-- | /See:/ 'modifyClusterIAMRolesResponse' smart constructor.
data ModifyClusterIAMRolesResponse = ModifyClusterIAMRolesResponse'
  { _mcirrsCluster        :: !(Maybe Cluster)
  , _mcirrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyClusterIAMRolesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcirrsCluster' - Undocumented member.
--
-- * 'mcirrsResponseStatus' - -- | The response status code.
modifyClusterIAMRolesResponse
    :: Int -- ^ 'mcirrsResponseStatus'
    -> ModifyClusterIAMRolesResponse
modifyClusterIAMRolesResponse pResponseStatus_ =
  ModifyClusterIAMRolesResponse'
    {_mcirrsCluster = Nothing, _mcirrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mcirrsCluster :: Lens' ModifyClusterIAMRolesResponse (Maybe Cluster)
mcirrsCluster = lens _mcirrsCluster (\ s a -> s{_mcirrsCluster = a})

-- | -- | The response status code.
mcirrsResponseStatus :: Lens' ModifyClusterIAMRolesResponse Int
mcirrsResponseStatus = lens _mcirrsResponseStatus (\ s a -> s{_mcirrsResponseStatus = a})

instance NFData ModifyClusterIAMRolesResponse where
