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
-- Module      : Network.AWS.OpsWorksCM.DisassociateNode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a node from an AWS OpsWorks CM server, and removes the node from the server's managed nodes. After a node is disassociated, the node key pair is no longer valid for accessing the configuration manager's API. For more information about how to associate a node, see 'AssociateNode' .
--
--
-- A node can can only be disassociated from a server that is in a @HEALTHY@ state. Otherwise, an @InvalidStateException@ is thrown. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.DisassociateNode
    (
    -- * Creating a Request
      disassociateNode
    , DisassociateNode
    -- * Request Lenses
    , dnEngineAttributes
    , dnServerName
    , dnNodeName

    -- * Destructuring the Response
    , disassociateNodeResponse
    , DisassociateNodeResponse
    -- * Response Lenses
    , dnrsNodeAssociationStatusToken
    , dnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateNode' smart constructor.
data DisassociateNode = DisassociateNode'
  { _dnEngineAttributes :: !(Maybe [EngineAttribute])
  , _dnServerName       :: !Text
  , _dnNodeName         :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnEngineAttributes' - Engine attributes that are used for disassociating the node. No attributes are required for Puppet.  __Attributes required in a DisassociateNode request for Chef__      * @CHEF_ORGANIZATION@ : The Chef organization with which the node was associated. By default only one organization named @default@ can exist.
--
-- * 'dnServerName' - The name of the server from which to disassociate the node.
--
-- * 'dnNodeName' - The name of the client node.
disassociateNode
    :: Text -- ^ 'dnServerName'
    -> Text -- ^ 'dnNodeName'
    -> DisassociateNode
disassociateNode pServerName_ pNodeName_ =
  DisassociateNode'
    { _dnEngineAttributes = Nothing
    , _dnServerName = pServerName_
    , _dnNodeName = pNodeName_
    }


-- | Engine attributes that are used for disassociating the node. No attributes are required for Puppet.  __Attributes required in a DisassociateNode request for Chef__      * @CHEF_ORGANIZATION@ : The Chef organization with which the node was associated. By default only one organization named @default@ can exist.
dnEngineAttributes :: Lens' DisassociateNode [EngineAttribute]
dnEngineAttributes = lens _dnEngineAttributes (\ s a -> s{_dnEngineAttributes = a}) . _Default . _Coerce

-- | The name of the server from which to disassociate the node.
dnServerName :: Lens' DisassociateNode Text
dnServerName = lens _dnServerName (\ s a -> s{_dnServerName = a})

-- | The name of the client node.
dnNodeName :: Lens' DisassociateNode Text
dnNodeName = lens _dnNodeName (\ s a -> s{_dnNodeName = a})

instance AWSRequest DisassociateNode where
        type Rs DisassociateNode = DisassociateNodeResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 DisassociateNodeResponse' <$>
                   (x .?> "NodeAssociationStatusToken") <*>
                     (pure (fromEnum s)))

instance Hashable DisassociateNode where

instance NFData DisassociateNode where

instance ToHeaders DisassociateNode where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.DisassociateNode" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateNode where
        toJSON DisassociateNode'{..}
          = object
              (catMaybes
                 [("EngineAttributes" .=) <$> _dnEngineAttributes,
                  Just ("ServerName" .= _dnServerName),
                  Just ("NodeName" .= _dnNodeName)])

instance ToPath DisassociateNode where
        toPath = const "/"

instance ToQuery DisassociateNode where
        toQuery = const mempty

-- | /See:/ 'disassociateNodeResponse' smart constructor.
data DisassociateNodeResponse = DisassociateNodeResponse'
  { _dnrsNodeAssociationStatusToken :: !(Maybe Text)
  , _dnrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnrsNodeAssociationStatusToken' - Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the disassociation request.
--
-- * 'dnrsResponseStatus' - -- | The response status code.
disassociateNodeResponse
    :: Int -- ^ 'dnrsResponseStatus'
    -> DisassociateNodeResponse
disassociateNodeResponse pResponseStatus_ =
  DisassociateNodeResponse'
    { _dnrsNodeAssociationStatusToken = Nothing
    , _dnrsResponseStatus = pResponseStatus_
    }


-- | Contains a token which can be passed to the @DescribeNodeAssociationStatus@ API call to get the status of the disassociation request.
dnrsNodeAssociationStatusToken :: Lens' DisassociateNodeResponse (Maybe Text)
dnrsNodeAssociationStatusToken = lens _dnrsNodeAssociationStatusToken (\ s a -> s{_dnrsNodeAssociationStatusToken = a})

-- | -- | The response status code.
dnrsResponseStatus :: Lens' DisassociateNodeResponse Int
dnrsResponseStatus = lens _dnrsResponseStatus (\ s a -> s{_dnrsResponseStatus = a})

instance NFData DisassociateNodeResponse where
