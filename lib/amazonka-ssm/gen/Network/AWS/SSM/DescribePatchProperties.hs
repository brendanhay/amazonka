{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribePatchProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the properties of available patches organized by product, product family, classification, severity, and other properties of available patches. You can use the reported properties in the filters you specify in requests for actions such as 'CreatePatchBaseline' , 'UpdatePatchBaseline' , 'DescribeAvailablePatches' , and 'DescribePatchBaselines' .
--
-- The following section lists the properties that can be used in filters for each major operating system type:
--
--     * AMAZON_LINUX
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * AMAZON_LINUX_2
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * CENTOS
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * DEBIAN
--
--     * Valid properties: PRODUCT, PRIORITY
--
--
--     * ORACLE_LINUX
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * REDHAT_ENTERPRISE_LINUX
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * SUSE
--
--     * Valid properties: PRODUCT, CLASSIFICATION, SEVERITY
--
--
--     * UBUNTU
--
--     * Valid properties: PRODUCT, PRIORITY
--
--
--     * WINDOWS
--
--     * Valid properties: PRODUCT, PRODUCT_FAMILY, CLASSIFICATION, MSRC_SEVERITY
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribePatchProperties
  ( -- * Creating a request
    DescribePatchProperties (..),
    mkDescribePatchProperties,

    -- ** Request lenses
    dppPatchSet,
    dppNextToken,
    dppMaxResults,
    dppOperatingSystem,
    dppProperty,

    -- * Destructuring the response
    DescribePatchPropertiesResponse (..),
    mkDescribePatchPropertiesResponse,

    -- ** Response lenses
    dpprsNextToken,
    dpprsProperties,
    dpprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDescribePatchProperties' smart constructor.
data DescribePatchProperties = DescribePatchProperties'
  { patchSet ::
      Lude.Maybe PatchSet,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    operatingSystem :: OperatingSystem,
    property :: PatchProperty
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePatchProperties' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'operatingSystem' - The operating system type for which to list patches.
-- * 'patchSet' - Indicates whether to list patches for the Windows operating system or for Microsoft applications. Not applicable for Linux operating systems.
-- * 'property' - The patch property for which you want to view patch details.
mkDescribePatchProperties ::
  -- | 'operatingSystem'
  OperatingSystem ->
  -- | 'property'
  PatchProperty ->
  DescribePatchProperties
mkDescribePatchProperties pOperatingSystem_ pProperty_ =
  DescribePatchProperties'
    { patchSet = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      operatingSystem = pOperatingSystem_,
      property = pProperty_
    }

-- | Indicates whether to list patches for the Windows operating system or for Microsoft applications. Not applicable for Linux operating systems.
--
-- /Note:/ Consider using 'patchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppPatchSet :: Lens.Lens' DescribePatchProperties (Lude.Maybe PatchSet)
dppPatchSet = Lens.lens (patchSet :: DescribePatchProperties -> Lude.Maybe PatchSet) (\s a -> s {patchSet = a} :: DescribePatchProperties)
{-# DEPRECATED dppPatchSet "Use generic-lens or generic-optics with 'patchSet' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppNextToken :: Lens.Lens' DescribePatchProperties (Lude.Maybe Lude.Text)
dppNextToken = Lens.lens (nextToken :: DescribePatchProperties -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePatchProperties)
{-# DEPRECATED dppNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppMaxResults :: Lens.Lens' DescribePatchProperties (Lude.Maybe Lude.Natural)
dppMaxResults = Lens.lens (maxResults :: DescribePatchProperties -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribePatchProperties)
{-# DEPRECATED dppMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The operating system type for which to list patches.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppOperatingSystem :: Lens.Lens' DescribePatchProperties OperatingSystem
dppOperatingSystem = Lens.lens (operatingSystem :: DescribePatchProperties -> OperatingSystem) (\s a -> s {operatingSystem = a} :: DescribePatchProperties)
{-# DEPRECATED dppOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | The patch property for which you want to view patch details.
--
-- /Note:/ Consider using 'property' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppProperty :: Lens.Lens' DescribePatchProperties PatchProperty
dppProperty = Lens.lens (property :: DescribePatchProperties -> PatchProperty) (\s a -> s {property = a} :: DescribePatchProperties)
{-# DEPRECATED dppProperty "Use generic-lens or generic-optics with 'property' instead." #-}

instance Page.AWSPager DescribePatchProperties where
  page rq rs
    | Page.stop (rs Lens.^. dpprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dpprsProperties) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dppNextToken Lens..~ rs Lens.^. dpprsNextToken

instance Lude.AWSRequest DescribePatchProperties where
  type Rs DescribePatchProperties = DescribePatchPropertiesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePatchPropertiesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Properties" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePatchProperties where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DescribePatchProperties" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePatchProperties where
  toJSON DescribePatchProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PatchSet" Lude..=) Lude.<$> patchSet,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("OperatingSystem" Lude..= operatingSystem),
            Lude.Just ("Property" Lude..= property)
          ]
      )

instance Lude.ToPath DescribePatchProperties where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePatchProperties where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePatchPropertiesResponse' smart constructor.
data DescribePatchPropertiesResponse = DescribePatchPropertiesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    properties ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (Lude.Text)
        ],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePatchPropertiesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token for the next set of items to return. (You use this token in the next call.)
-- * 'properties' - A list of the properties for patches matching the filter request parameters.
-- * 'responseStatus' - The response status code.
mkDescribePatchPropertiesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePatchPropertiesResponse
mkDescribePatchPropertiesResponse pResponseStatus_ =
  DescribePatchPropertiesResponse'
    { nextToken = Lude.Nothing,
      properties = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. (You use this token in the next call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsNextToken :: Lens.Lens' DescribePatchPropertiesResponse (Lude.Maybe Lude.Text)
dpprsNextToken = Lens.lens (nextToken :: DescribePatchPropertiesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribePatchPropertiesResponse)
{-# DEPRECATED dpprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of the properties for patches matching the filter request parameters.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsProperties :: Lens.Lens' DescribePatchPropertiesResponse (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
dpprsProperties = Lens.lens (properties :: DescribePatchPropertiesResponse -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {properties = a} :: DescribePatchPropertiesResponse)
{-# DEPRECATED dpprsProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprsResponseStatus :: Lens.Lens' DescribePatchPropertiesResponse Lude.Int
dpprsResponseStatus = Lens.lens (responseStatus :: DescribePatchPropertiesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePatchPropertiesResponse)
{-# DEPRECATED dpprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
