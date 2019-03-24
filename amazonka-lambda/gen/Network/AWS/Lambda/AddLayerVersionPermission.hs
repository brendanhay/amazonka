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
-- Module      : Network.AWS.Lambda.AddLayerVersionPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds permissions to the resource-based policy of a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . Use this action to grant layer usage permission to other accounts. You can grant permission to a single account, all AWS accounts, or all accounts in an organization.
--
--
-- To revoke permission, call 'RemoveLayerVersionPermission' with the statement ID that you specified when you added it.
--
module Network.AWS.Lambda.AddLayerVersionPermission
    (
    -- * Creating a Request
      addLayerVersionPermission
    , AddLayerVersionPermission
    -- * Request Lenses
    , alvpRevisionId
    , alvpOrganizationId
    , alvpLayerName
    , alvpVersionNumber
    , alvpStatementId
    , alvpAction
    , alvpPrincipal

    -- * Destructuring the Response
    , addLayerVersionPermissionResponse
    , AddLayerVersionPermissionResponse
    -- * Response Lenses
    , alvprsStatement
    , alvprsRevisionId
    , alvprsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addLayerVersionPermission' smart constructor.
data AddLayerVersionPermission = AddLayerVersionPermission'
  { _alvpRevisionId     :: !(Maybe Text)
  , _alvpOrganizationId :: !(Maybe Text)
  , _alvpLayerName      :: !Text
  , _alvpVersionNumber  :: !Integer
  , _alvpStatementId    :: !Text
  , _alvpAction         :: !Text
  , _alvpPrincipal      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddLayerVersionPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alvpRevisionId' - Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- * 'alvpOrganizationId' - With the principal set to @*@ , grant permission to all accounts in the specified organization.
--
-- * 'alvpLayerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- * 'alvpVersionNumber' - The version number.
--
-- * 'alvpStatementId' - An identifier that distinguishes the policy from others on the same layer version.
--
-- * 'alvpAction' - The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
--
-- * 'alvpPrincipal' - An account ID, or @*@ to grant permission to all AWS accounts.
addLayerVersionPermission
    :: Text -- ^ 'alvpLayerName'
    -> Integer -- ^ 'alvpVersionNumber'
    -> Text -- ^ 'alvpStatementId'
    -> Text -- ^ 'alvpAction'
    -> Text -- ^ 'alvpPrincipal'
    -> AddLayerVersionPermission
addLayerVersionPermission pLayerName_ pVersionNumber_ pStatementId_ pAction_ pPrincipal_ =
  AddLayerVersionPermission'
    { _alvpRevisionId = Nothing
    , _alvpOrganizationId = Nothing
    , _alvpLayerName = pLayerName_
    , _alvpVersionNumber = pVersionNumber_
    , _alvpStatementId = pStatementId_
    , _alvpAction = pAction_
    , _alvpPrincipal = pPrincipal_
    }


-- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
alvpRevisionId :: Lens' AddLayerVersionPermission (Maybe Text)
alvpRevisionId = lens _alvpRevisionId (\ s a -> s{_alvpRevisionId = a})

-- | With the principal set to @*@ , grant permission to all accounts in the specified organization.
alvpOrganizationId :: Lens' AddLayerVersionPermission (Maybe Text)
alvpOrganizationId = lens _alvpOrganizationId (\ s a -> s{_alvpOrganizationId = a})

-- | The name or Amazon Resource Name (ARN) of the layer.
alvpLayerName :: Lens' AddLayerVersionPermission Text
alvpLayerName = lens _alvpLayerName (\ s a -> s{_alvpLayerName = a})

-- | The version number.
alvpVersionNumber :: Lens' AddLayerVersionPermission Integer
alvpVersionNumber = lens _alvpVersionNumber (\ s a -> s{_alvpVersionNumber = a})

-- | An identifier that distinguishes the policy from others on the same layer version.
alvpStatementId :: Lens' AddLayerVersionPermission Text
alvpStatementId = lens _alvpStatementId (\ s a -> s{_alvpStatementId = a})

-- | The API action that grants access to the layer. For example, @lambda:GetLayerVersion@ .
alvpAction :: Lens' AddLayerVersionPermission Text
alvpAction = lens _alvpAction (\ s a -> s{_alvpAction = a})

-- | An account ID, or @*@ to grant permission to all AWS accounts.
alvpPrincipal :: Lens' AddLayerVersionPermission Text
alvpPrincipal = lens _alvpPrincipal (\ s a -> s{_alvpPrincipal = a})

instance AWSRequest AddLayerVersionPermission where
        type Rs AddLayerVersionPermission =
             AddLayerVersionPermissionResponse
        request = postJSON lambda
        response
          = receiveJSON
              (\ s h x ->
                 AddLayerVersionPermissionResponse' <$>
                   (x .?> "Statement") <*> (x .?> "RevisionId") <*>
                     (pure (fromEnum s)))

instance Hashable AddLayerVersionPermission where

instance NFData AddLayerVersionPermission where

instance ToHeaders AddLayerVersionPermission where
        toHeaders = const mempty

instance ToJSON AddLayerVersionPermission where
        toJSON AddLayerVersionPermission'{..}
          = object
              (catMaybes
                 [("OrganizationId" .=) <$> _alvpOrganizationId,
                  Just ("StatementId" .= _alvpStatementId),
                  Just ("Action" .= _alvpAction),
                  Just ("Principal" .= _alvpPrincipal)])

instance ToPath AddLayerVersionPermission where
        toPath AddLayerVersionPermission'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _alvpLayerName,
               "/versions/", toBS _alvpVersionNumber, "/policy"]

instance ToQuery AddLayerVersionPermission where
        toQuery AddLayerVersionPermission'{..}
          = mconcat ["RevisionId" =: _alvpRevisionId]

-- | /See:/ 'addLayerVersionPermissionResponse' smart constructor.
data AddLayerVersionPermissionResponse = AddLayerVersionPermissionResponse'
  { _alvprsStatement      :: !(Maybe Text)
  , _alvprsRevisionId     :: !(Maybe Text)
  , _alvprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddLayerVersionPermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alvprsStatement' - The permission statement.
--
-- * 'alvprsRevisionId' - A unique identifier for the current revision of the policy.
--
-- * 'alvprsResponseStatus' - -- | The response status code.
addLayerVersionPermissionResponse
    :: Int -- ^ 'alvprsResponseStatus'
    -> AddLayerVersionPermissionResponse
addLayerVersionPermissionResponse pResponseStatus_ =
  AddLayerVersionPermissionResponse'
    { _alvprsStatement = Nothing
    , _alvprsRevisionId = Nothing
    , _alvprsResponseStatus = pResponseStatus_
    }


-- | The permission statement.
alvprsStatement :: Lens' AddLayerVersionPermissionResponse (Maybe Text)
alvprsStatement = lens _alvprsStatement (\ s a -> s{_alvprsStatement = a})

-- | A unique identifier for the current revision of the policy.
alvprsRevisionId :: Lens' AddLayerVersionPermissionResponse (Maybe Text)
alvprsRevisionId = lens _alvprsRevisionId (\ s a -> s{_alvprsRevisionId = a})

-- | -- | The response status code.
alvprsResponseStatus :: Lens' AddLayerVersionPermissionResponse Int
alvprsResponseStatus = lens _alvprsResponseStatus (\ s a -> s{_alvprsResponseStatus = a})

instance NFData AddLayerVersionPermissionResponse
         where
