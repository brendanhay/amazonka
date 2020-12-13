{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'WebACL' that is specified by @WebACLId@ .
module Network.AWS.WAF.GetWebACL
  ( -- * Creating a request
    GetWebACL (..),
    mkGetWebACL,

    -- ** Request lenses
    gwaWebACLId,

    -- * Destructuring the response
    GetWebACLResponse (..),
    mkGetWebACLResponse,

    -- ** Response lenses
    gwarsWebACL,
    gwarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAF.Types

-- | /See:/ 'mkGetWebACL' smart constructor.
newtype GetWebACL = GetWebACL'
  { -- | The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
    webACLId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWebACL' with the minimum fields required to make a request.
--
-- * 'webACLId' - The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
mkGetWebACL ::
  -- | 'webACLId'
  Lude.Text ->
  GetWebACL
mkGetWebACL pWebACLId_ = GetWebACL' {webACLId = pWebACLId_}

-- | The @WebACLId@ of the 'WebACL' that you want to get. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwaWebACLId :: Lens.Lens' GetWebACL Lude.Text
gwaWebACLId = Lens.lens (webACLId :: GetWebACL -> Lude.Text) (\s a -> s {webACLId = a} :: GetWebACL)
{-# DEPRECATED gwaWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

instance Lude.AWSRequest GetWebACL where
  type Rs GetWebACL = GetWebACLResponse
  request = Req.postJSON wafService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetWebACLResponse'
            Lude.<$> (x Lude..?> "WebACL") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSWAF_20150824.GetWebACL" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetWebACL where
  toJSON GetWebACL' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WebACLId" Lude..= webACLId)])

instance Lude.ToPath GetWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery GetWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetWebACLResponse' smart constructor.
data GetWebACLResponse = GetWebACLResponse'
  { -- | Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:
    --
    --
    --     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@
    --
    --
    --     * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@
    --
    --
    --     * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@
    --
    --
    --     * @Action@ : Contains @Type@
    webACL :: Lude.Maybe WebACL,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetWebACLResponse' with the minimum fields required to make a request.
--
-- * 'webACL' - Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:
--
--
--     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@
--
--
--     * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@
--
--
--     * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@
--
--
--     * @Action@ : Contains @Type@
--
--
-- * 'responseStatus' - The response status code.
mkGetWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetWebACLResponse
mkGetWebACLResponse pResponseStatus_ =
  GetWebACLResponse'
    { webACL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the 'WebACL' that you specified in the @GetWebACL@ request. For more information, see the following topics:
--
--
--     * 'WebACL' : Contains @DefaultAction@ , @MetricName@ , @Name@ , an array of @Rule@ objects, and @WebACLId@
--
--
--     * @DefaultAction@ (Data type is 'WafAction' ): Contains @Type@
--
--
--     * @Rules@ : Contains an array of @ActivatedRule@ objects, which contain @Action@ , @Priority@ , and @RuleId@
--
--
--     * @Action@ : Contains @Type@
--
--
--
-- /Note:/ Consider using 'webACL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwarsWebACL :: Lens.Lens' GetWebACLResponse (Lude.Maybe WebACL)
gwarsWebACL = Lens.lens (webACL :: GetWebACLResponse -> Lude.Maybe WebACL) (\s a -> s {webACL = a} :: GetWebACLResponse)
{-# DEPRECATED gwarsWebACL "Use generic-lens or generic-optics with 'webACL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gwarsResponseStatus :: Lens.Lens' GetWebACLResponse Lude.Int
gwarsResponseStatus = Lens.lens (responseStatus :: GetWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetWebACLResponse)
{-# DEPRECATED gwarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
