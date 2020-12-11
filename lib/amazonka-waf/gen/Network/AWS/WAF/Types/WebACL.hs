-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WebACL
  ( WebACL (..),

    -- * Smart constructor
    mkWebACL,

    -- * Lenses
    waMetricName,
    waName,
    waWebACLARN,
    waWebACLId,
    waDefaultAction,
    waRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.WafAction

-- | Contains the @Rules@ that identify the requests that you want to allow, block, or count. In a @WebACL@ , you also specify a default action (@ALLOW@ or @BLOCK@ ), and the action for each @Rule@ that you add to a @WebACL@ , for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the @WebACL@ with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one @Rule@ to a @WebACL@ , a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see 'UpdateWebACL' .
--
-- /See:/ 'mkWebACL' smart constructor.
data WebACL = WebACL'
  { metricName :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    webACLARN :: Lude.Maybe Lude.Text,
    webACLId :: Lude.Text,
    defaultAction :: WafAction,
    rules :: [ActivatedRule]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebACL' with the minimum fields required to make a request.
--
-- * 'defaultAction' - The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
-- * 'metricName' - A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
-- * 'name' - A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
-- * 'rules' - An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
-- * 'webACLARN' - Tha Amazon Resource Name (ARN) of the web ACL.
-- * 'webACLId' - A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
--
-- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
mkWebACL ::
  -- | 'webACLId'
  Lude.Text ->
  -- | 'defaultAction'
  WafAction ->
  WebACL
mkWebACL pWebACLId_ pDefaultAction_ =
  WebACL'
    { metricName = Lude.Nothing,
      name = Lude.Nothing,
      webACLARN = Lude.Nothing,
      webACLId = pWebACLId_,
      defaultAction = pDefaultAction_,
      rules = Lude.mempty
    }

-- | A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waMetricName :: Lens.Lens' WebACL (Lude.Maybe Lude.Text)
waMetricName = Lens.lens (metricName :: WebACL -> Lude.Maybe Lude.Text) (\s a -> s {metricName = a} :: WebACL)
{-# DEPRECATED waMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waName :: Lens.Lens' WebACL (Lude.Maybe Lude.Text)
waName = Lens.lens (name :: WebACL -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WebACL)
{-# DEPRECATED waName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tha Amazon Resource Name (ARN) of the web ACL.
--
-- /Note:/ Consider using 'webACLARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waWebACLARN :: Lens.Lens' WebACL (Lude.Maybe Lude.Text)
waWebACLARN = Lens.lens (webACLARN :: WebACL -> Lude.Maybe Lude.Text) (\s a -> s {webACLARN = a} :: WebACL)
{-# DEPRECATED waWebACLARN "Use generic-lens or generic-optics with 'webACLARN' instead." #-}

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
--
-- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waWebACLId :: Lens.Lens' WebACL Lude.Text
waWebACLId = Lens.lens (webACLId :: WebACL -> Lude.Text) (\s a -> s {webACLId = a} :: WebACL)
{-# DEPRECATED waWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waDefaultAction :: Lens.Lens' WebACL WafAction
waDefaultAction = Lens.lens (defaultAction :: WebACL -> WafAction) (\s a -> s {defaultAction = a} :: WebACL)
{-# DEPRECATED waDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waRules :: Lens.Lens' WebACL [ActivatedRule]
waRules = Lens.lens (rules :: WebACL -> [ActivatedRule]) (\s a -> s {rules = a} :: WebACL)
{-# DEPRECATED waRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Lude.FromJSON WebACL where
  parseJSON =
    Lude.withObject
      "WebACL"
      ( \x ->
          WebACL'
            Lude.<$> (x Lude..:? "MetricName")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "WebACLArn")
            Lude.<*> (x Lude..: "WebACLId")
            Lude.<*> (x Lude..: "DefaultAction")
            Lude.<*> (x Lude..:? "Rules" Lude..!= Lude.mempty)
      )
