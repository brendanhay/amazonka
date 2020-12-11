{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.AddAttributesToFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns attributes (key and value pairs) to the findings that are specified by the ARNs of the findings.
module Network.AWS.Inspector.AddAttributesToFindings
  ( -- * Creating a request
    AddAttributesToFindings (..),
    mkAddAttributesToFindings,

    -- ** Request lenses
    aatfFindingARNs,
    aatfAttributes,

    -- * Destructuring the response
    AddAttributesToFindingsResponse (..),
    mkAddAttributesToFindingsResponse,

    -- ** Response lenses
    aatfrsResponseStatus,
    aatfrsFailedItems,
  )
where

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddAttributesToFindings' smart constructor.
data AddAttributesToFindings = AddAttributesToFindings'
  { findingARNs ::
      Lude.NonEmpty Lude.Text,
    attributes :: [Attribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddAttributesToFindings' with the minimum fields required to make a request.
--
-- * 'attributes' - The array of attributes that you want to assign to specified findings.
-- * 'findingARNs' - The ARNs that specify the findings that you want to assign attributes to.
mkAddAttributesToFindings ::
  -- | 'findingARNs'
  Lude.NonEmpty Lude.Text ->
  AddAttributesToFindings
mkAddAttributesToFindings pFindingARNs_ =
  AddAttributesToFindings'
    { findingARNs = pFindingARNs_,
      attributes = Lude.mempty
    }

-- | The ARNs that specify the findings that you want to assign attributes to.
--
-- /Note:/ Consider using 'findingARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfFindingARNs :: Lens.Lens' AddAttributesToFindings (Lude.NonEmpty Lude.Text)
aatfFindingARNs = Lens.lens (findingARNs :: AddAttributesToFindings -> Lude.NonEmpty Lude.Text) (\s a -> s {findingARNs = a} :: AddAttributesToFindings)
{-# DEPRECATED aatfFindingARNs "Use generic-lens or generic-optics with 'findingARNs' instead." #-}

-- | The array of attributes that you want to assign to specified findings.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfAttributes :: Lens.Lens' AddAttributesToFindings [Attribute]
aatfAttributes = Lens.lens (attributes :: AddAttributesToFindings -> [Attribute]) (\s a -> s {attributes = a} :: AddAttributesToFindings)
{-# DEPRECATED aatfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest AddAttributesToFindings where
  type Rs AddAttributesToFindings = AddAttributesToFindingsResponse
  request = Req.postJSON inspectorService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddAttributesToFindingsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "failedItems" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders AddAttributesToFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("InspectorService.AddAttributesToFindings" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddAttributesToFindings where
  toJSON AddAttributesToFindings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("findingArns" Lude..= findingARNs),
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )

instance Lude.ToPath AddAttributesToFindings where
  toPath = Lude.const "/"

instance Lude.ToQuery AddAttributesToFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddAttributesToFindingsResponse' smart constructor.
data AddAttributesToFindingsResponse = AddAttributesToFindingsResponse'
  { responseStatus ::
      Lude.Int,
    failedItems ::
      Lude.HashMap
        Lude.Text
        (FailedItemDetails)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddAttributesToFindingsResponse' with the minimum fields required to make a request.
--
-- * 'failedItems' - Attribute details that cannot be described. An error code is provided for each failed item.
-- * 'responseStatus' - The response status code.
mkAddAttributesToFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddAttributesToFindingsResponse
mkAddAttributesToFindingsResponse pResponseStatus_ =
  AddAttributesToFindingsResponse'
    { responseStatus =
        pResponseStatus_,
      failedItems = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfrsResponseStatus :: Lens.Lens' AddAttributesToFindingsResponse Lude.Int
aatfrsResponseStatus = Lens.lens (responseStatus :: AddAttributesToFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddAttributesToFindingsResponse)
{-# DEPRECATED aatfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Attribute details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfrsFailedItems :: Lens.Lens' AddAttributesToFindingsResponse (Lude.HashMap Lude.Text (FailedItemDetails))
aatfrsFailedItems = Lens.lens (failedItems :: AddAttributesToFindingsResponse -> Lude.HashMap Lude.Text (FailedItemDetails)) (\s a -> s {failedItems = a} :: AddAttributesToFindingsResponse)
{-# DEPRECATED aatfrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}
