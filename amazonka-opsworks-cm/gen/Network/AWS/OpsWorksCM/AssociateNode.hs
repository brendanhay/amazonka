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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.OpsWorksCM.AssociateNode
    (
    -- * Creating a Request
      associateNode
    , AssociateNode
    -- * Request Lenses
    , anEngineAttributes
    , anServerName
    , anNodeName

    -- * Destructuring the Response
    , associateNodeResponse
    , AssociateNodeResponse
    -- * Response Lenses
    , anrsNodeAssociationStatusToken
    , anrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.OpsWorksCM.Types
import           Network.AWS.OpsWorksCM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateNode' smart constructor.
data AssociateNode = AssociateNode'
    { _anEngineAttributes :: !(Maybe [EngineAttribute])
    , _anServerName       :: !Text
    , _anNodeName         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociateNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anEngineAttributes' - Undocumented member.
--
-- * 'anServerName' - Undocumented member.
--
-- * 'anNodeName' - Undocumented member.
associateNode
    :: Text -- ^ 'anServerName'
    -> Text -- ^ 'anNodeName'
    -> AssociateNode
associateNode pServerName_ pNodeName_ =
    AssociateNode'
    { _anEngineAttributes = Nothing
    , _anServerName = pServerName_
    , _anNodeName = pNodeName_
    }

-- | Undocumented member.
anEngineAttributes :: Lens' AssociateNode [EngineAttribute]
anEngineAttributes = lens _anEngineAttributes (\ s a -> s{_anEngineAttributes = a}) . _Default . _Coerce;

-- | Undocumented member.
anServerName :: Lens' AssociateNode Text
anServerName = lens _anServerName (\ s a -> s{_anServerName = a});

-- | Undocumented member.
anNodeName :: Lens' AssociateNode Text
anNodeName = lens _anNodeName (\ s a -> s{_anNodeName = a});

instance AWSRequest AssociateNode where
        type Rs AssociateNode = AssociateNodeResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 AssociateNodeResponse' <$>
                   (x .?> "NodeAssociationStatusToken") <*>
                     (pure (fromEnum s)))

instance Hashable AssociateNode

instance NFData AssociateNode

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
                 [("EngineAttributes" .=) <$> _anEngineAttributes,
                  Just ("ServerName" .= _anServerName),
                  Just ("NodeName" .= _anNodeName)])

instance ToPath AssociateNode where
        toPath = const "/"

instance ToQuery AssociateNode where
        toQuery = const mempty

-- | /See:/ 'associateNodeResponse' smart constructor.
data AssociateNodeResponse = AssociateNodeResponse'
    { _anrsNodeAssociationStatusToken :: !(Maybe Text)
    , _anrsResponseStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AssociateNodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anrsNodeAssociationStatusToken' - Undocumented member.
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

-- | Undocumented member.
anrsNodeAssociationStatusToken :: Lens' AssociateNodeResponse (Maybe Text)
anrsNodeAssociationStatusToken = lens _anrsNodeAssociationStatusToken (\ s a -> s{_anrsNodeAssociationStatusToken = a});

-- | -- | The response status code.
anrsResponseStatus :: Lens' AssociateNodeResponse Int
anrsResponseStatus = lens _anrsResponseStatus (\ s a -> s{_anrsResponseStatus = a});

instance NFData AssociateNodeResponse
