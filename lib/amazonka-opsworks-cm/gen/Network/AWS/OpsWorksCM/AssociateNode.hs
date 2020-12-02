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
-- Module      : Network.AWS.OpsWorksCM.AssociateNode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a new node with the server. For more information about how to disassociate a node, see 'DisassociateNode' .
--
--
-- On a Chef server: This command is an alternative to @knife bootstrap@ .
--
-- Example (Chef): @aws opsworks-cm associate-node --server-name /MyServer/ --node-name /MyManagedNode/ --engine-attributes "Name=/CHEF_ORGANIZATION/ ,Value=default" "Name=/CHEF_NODE_PUBLIC_KEY/ ,Value=/public-key-pem/ "@
--
-- On a Puppet server, this command is an alternative to the @puppet cert sign@ command that signs a Puppet node CSR.
--
-- Example (Chef): @aws opsworks-cm associate-node --server-name /MyServer/ --node-name /MyManagedNode/ --engine-attributes "Name=/PUPPET_NODE_CSR/ ,Value=/csr-pem/ "@
--
-- A node can can only be associated with servers that are in a @HEALTHY@ state. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid. The AssociateNode API call can be integrated into Auto Scaling configurations, AWS Cloudformation templates, or the user data of a server's instance.
--
module Network.AWS.OpsWorksCM.AssociateNode
    (
    -- * Creating a Request
      associateNode
    , AssociateNode
    -- * Request Lenses
    , anServerName
    , anNodeName
    , anEngineAttributes

    -- * Destructuring the Response
    , associateNodeResponse
    , AssociateNodeResponse
    -- * Response Lenses
    , anrsNodeAssociationStatusToken
    , anrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateNode' smart constructor.
data AssociateNode = AssociateNode'
  { _anServerName       :: !Text
  , _anNodeName         :: !Text
  , _anEngineAttributes :: ![EngineAttribute]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anServerName' - The name of the server with which to associate the node.
--
-- * 'anNodeName' - The name of the node.
--
-- * 'anEngineAttributes' - Engine attributes used for associating the node.  __Attributes accepted in a AssociateNode request for Chef__      * @CHEF_ORGANIZATION@ : The Chef organization with which the node is associated. By default only one organization named @default@ can exist.      * @CHEF_NODE_PUBLIC_KEY@ : A PEM-formatted public key. This key is required for the @chef-client@ agent to access the Chef API.  __Attributes accepted in a AssociateNode request for Puppet__      * @PUPPET_NODE_CSR@ : A PEM-formatted certificate-signing request (CSR) that is created by the node.
associateNode
    :: Text -- ^ 'anServerName'
    -> Text -- ^ 'anNodeName'
    -> AssociateNode
associateNode pServerName_ pNodeName_ =
  AssociateNode'
    { _anServerName = pServerName_
    , _anNodeName = pNodeName_
    , _anEngineAttributes = mempty
    }


-- | The name of the server with which to associate the node.
anServerName :: Lens' AssociateNode Text
anServerName = lens _anServerName (\ s a -> s{_anServerName = a})

-- | The name of the node.
anNodeName :: Lens' AssociateNode Text
anNodeName = lens _anNodeName (\ s a -> s{_anNodeName = a})

-- | Engine attributes used for associating the node.  __Attributes accepted in a AssociateNode request for Chef__      * @CHEF_ORGANIZATION@ : The Chef organization with which the node is associated. By default only one organization named @default@ can exist.      * @CHEF_NODE_PUBLIC_KEY@ : A PEM-formatted public key. This key is required for the @chef-client@ agent to access the Chef API.  __Attributes accepted in a AssociateNode request for Puppet__      * @PUPPET_NODE_CSR@ : A PEM-formatted certificate-signing request (CSR) that is created by the node.
anEngineAttributes :: Lens' AssociateNode [EngineAttribute]
anEngineAttributes = lens _anEngineAttributes (\ s a -> s{_anEngineAttributes = a}) . _Coerce

instance AWSRequest AssociateNode where
        type Rs AssociateNode = AssociateNodeResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 AssociateNodeResponse' <$>
                   (x .?> "NodeAssociationStatusToken") <*>
                     (pure (fromEnum s)))

instance Hashable AssociateNode where

instance NFData AssociateNode where

instance ToHeaders AssociateNode where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.AssociateNode" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateNode where
        toJSON AssociateNode'{..}
          = object
              (catMaybes
                 [Just ("ServerName" .= _anServerName),
                  Just ("NodeName" .= _anNodeName),
                  Just ("EngineAttributes" .= _anEngineAttributes)])

instance ToPath AssociateNode where
        toPath = const "/"

instance ToQuery AssociateNode where
        toQuery = const mempty

-- | /See:/ 'associateNodeResponse' smart constructor.
data AssociateNodeResponse = AssociateNodeResponse'
  { _anrsNodeAssociationStatusToken :: !(Maybe Text)
  , _anrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anrsNodeAssociationStatusToken' - Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request.
--
-- * 'anrsResponseStatus' - -- | The response status code.
associateNodeResponse
    :: Int -- ^ 'anrsResponseStatus'
    -> AssociateNodeResponse
associateNodeResponse pResponseStatus_ =
  AssociateNodeResponse'
    { _anrsNodeAssociationStatusToken = Nothing
    , _anrsResponseStatus = pResponseStatus_
    }


-- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the association request.
anrsNodeAssociationStatusToken :: Lens' AssociateNodeResponse (Maybe Text)
anrsNodeAssociationStatusToken = lens _anrsNodeAssociationStatusToken (\ s a -> s{_anrsNodeAssociationStatusToken = a})

-- | -- | The response status code.
anrsResponseStatus :: Lens' AssociateNodeResponse Int
anrsResponseStatus = lens _anrsResponseStatus (\ s a -> s{_anrsResponseStatus = a})

instance NFData AssociateNodeResponse where
