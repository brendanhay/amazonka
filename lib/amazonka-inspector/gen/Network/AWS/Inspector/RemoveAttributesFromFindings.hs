{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.RemoveAttributesFromFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes entire attributes (key and value pairs) from the findings that are specified by the ARNs of the findings where an attribute with the specified key exists.
module Network.AWS.Inspector.RemoveAttributesFromFindings
  ( -- * Creating a request
    RemoveAttributesFromFindings (..),
    mkRemoveAttributesFromFindings,

    -- ** Request lenses
    raffFindingARNs,
    raffAttributeKeys,

    -- * Destructuring the response
    RemoveAttributesFromFindingsResponse (..),
    mkRemoveAttributesFromFindingsResponse,

    -- ** Response lenses
    raffrsFailedItems,
    raffrsResponseStatus,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
  { -- | The ARNs that specify the findings that you want to remove attributes from.
    findingARNs :: Lude.NonEmpty Lude.Text,
    -- | The array of attribute keys that you want to remove from specified findings.
    attributeKeys :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAttributesFromFindings' with the minimum fields required to make a request.
--
-- * 'findingARNs' - The ARNs that specify the findings that you want to remove attributes from.
-- * 'attributeKeys' - The array of attribute keys that you want to remove from specified findings.
mkRemoveAttributesFromFindings ::
  -- | 'findingARNs'
  Lude.NonEmpty Lude.Text ->
  RemoveAttributesFromFindings
mkRemoveAttributesFromFindings pFindingARNs_ =
  RemoveAttributesFromFindings'
    { findingARNs = pFindingARNs_,
      attributeKeys = Lude.mempty
    }

-- | The ARNs that specify the findings that you want to remove attributes from.
--
-- /Note:/ Consider using 'findingARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffFindingARNs :: Lens.Lens' RemoveAttributesFromFindings (Lude.NonEmpty Lude.Text)
raffFindingARNs = Lens.lens (findingARNs :: RemoveAttributesFromFindings -> Lude.NonEmpty Lude.Text) (\s a -> s {findingARNs = a} :: RemoveAttributesFromFindings)
{-# DEPRECATED raffFindingARNs "Use generic-lens or generic-optics with 'findingARNs' instead." #-}

-- | The array of attribute keys that you want to remove from specified findings.
--
-- /Note:/ Consider using 'attributeKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffAttributeKeys :: Lens.Lens' RemoveAttributesFromFindings [Lude.Text]
raffAttributeKeys = Lens.lens (attributeKeys :: RemoveAttributesFromFindings -> [Lude.Text]) (\s a -> s {attributeKeys = a} :: RemoveAttributesFromFindings)
{-# DEPRECATED raffAttributeKeys "Use generic-lens or generic-optics with 'attributeKeys' instead." #-}

instance Lude.AWSRequest RemoveAttributesFromFindings where
  type
    Rs RemoveAttributesFromFindings =
      RemoveAttributesFromFindingsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          RemoveAttributesFromFindingsResponse'
            Lude.<$> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveAttributesFromFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "InspectorService.RemoveAttributesFromFindings" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveAttributesFromFindings where
  toJSON RemoveAttributesFromFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("findingArns" Lude..= findingARNs),
            Lude.Just ("attributeKeys" Lude..= attributeKeys)
          ]
      )

instance Lude.ToPath RemoveAttributesFromFindings where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveAttributesFromFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
  { -- | Attributes details that cannot be described. An error code is provided for each failed item.
    failedItems :: Lude.HashMap Lude.Text (FailedItemDetails),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAttributesFromFindingsResponse' with the minimum fields required to make a request.
--
-- * 'failedItems' - Attributes details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
mkRemoveAttributesFromFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveAttributesFromFindingsResponse
mkRemoveAttributesFromFindingsResponse pResponseStatus_ =
  RemoveAttributesFromFindingsResponse'
    { failedItems = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Attributes details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffrsFailedItems :: Lens.Lens' RemoveAttributesFromFindingsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
raffrsFailedItems = Lens.lens (failedItems :: RemoveAttributesFromFindingsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: RemoveAttributesFromFindingsResponse)
{-# DEPRECATED raffrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffrsResponseStatus :: Lens.Lens' RemoveAttributesFromFindingsResponse Lude.Int
raffrsResponseStatus = Lens.lens (responseStatus :: RemoveAttributesFromFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveAttributesFromFindingsResponse)
{-# DEPRECATED raffrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
