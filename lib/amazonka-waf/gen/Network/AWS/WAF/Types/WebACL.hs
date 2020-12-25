{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    waclWebACLId,
    waclDefaultAction,
    waclRules,
    waclMetricName,
    waclName,
    waclWebACLArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.ActivatedRule as Types
import qualified Network.AWS.WAF.Types.MetricName as Types
import qualified Network.AWS.WAF.Types.ResourceId as Types
import qualified Network.AWS.WAF.Types.ResourceName as Types
import qualified Network.AWS.WAF.Types.WafAction as Types
import qualified Network.AWS.WAF.Types.WebACLArn as Types

-- | Contains the @Rules@ that identify the requests that you want to allow, block, or count. In a @WebACL@ , you also specify a default action (@ALLOW@ or @BLOCK@ ), and the action for each @Rule@ that you add to a @WebACL@ , for example, block requests from specified IP addresses or block requests from specified referrers. You also associate the @WebACL@ with a CloudFront distribution to identify the requests that you want AWS WAF to filter. If you add more than one @Rule@ to a @WebACL@ , a request needs to match only one of the specifications to be allowed, blocked, or counted. For more information, see 'UpdateWebACL' .
--
-- /See:/ 'mkWebACL' smart constructor.
data WebACL = WebACL'
  { -- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
    --
    -- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
    webACLId :: Types.ResourceId,
    -- | The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
    defaultAction :: Types.WafAction,
    -- | An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
    rules :: [Types.ActivatedRule],
    -- | A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
    metricName :: Core.Maybe Types.MetricName,
    -- | A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
    name :: Core.Maybe Types.ResourceName,
    -- | Tha Amazon Resource Name (ARN) of the web ACL.
    webACLArn :: Core.Maybe Types.WebACLArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebACL' value with any optional fields omitted.
mkWebACL ::
  -- | 'webACLId'
  Types.ResourceId ->
  -- | 'defaultAction'
  Types.WafAction ->
  WebACL
mkWebACL webACLId defaultAction =
  WebACL'
    { webACLId,
      defaultAction,
      rules = Core.mempty,
      metricName = Core.Nothing,
      name = Core.Nothing,
      webACLArn = Core.Nothing
    }

-- | A unique identifier for a @WebACL@ . You use @WebACLId@ to get information about a @WebACL@ (see 'GetWebACL' ), update a @WebACL@ (see 'UpdateWebACL' ), and delete a @WebACL@ from AWS WAF (see 'DeleteWebACL' ).
--
-- @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclWebACLId :: Lens.Lens' WebACL Types.ResourceId
waclWebACLId = Lens.field @"webACLId"
{-# DEPRECATED waclWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

-- | The action to perform if none of the @Rules@ contained in the @WebACL@ match. The action is specified by the 'WafAction' object.
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclDefaultAction :: Lens.Lens' WebACL Types.WafAction
waclDefaultAction = Lens.field @"defaultAction"
{-# DEPRECATED waclDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | An array that contains the action for each @Rule@ in a @WebACL@ , the priority of the @Rule@ , and the ID of the @Rule@ .
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclRules :: Lens.Lens' WebACL [Types.ActivatedRule]
waclRules = Lens.field @"rules"
{-# DEPRECATED waclRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | A friendly name or description for the metrics for this @WebACL@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change @MetricName@ after you create the @WebACL@ .
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclMetricName :: Lens.Lens' WebACL (Core.Maybe Types.MetricName)
waclMetricName = Lens.field @"metricName"
{-# DEPRECATED waclMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | A friendly name or description of the @WebACL@ . You can't change the name of a @WebACL@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclName :: Lens.Lens' WebACL (Core.Maybe Types.ResourceName)
waclName = Lens.field @"name"
{-# DEPRECATED waclName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tha Amazon Resource Name (ARN) of the web ACL.
--
-- /Note:/ Consider using 'webACLArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waclWebACLArn :: Lens.Lens' WebACL (Core.Maybe Types.WebACLArn)
waclWebACLArn = Lens.field @"webACLArn"
{-# DEPRECATED waclWebACLArn "Use generic-lens or generic-optics with 'webACLArn' instead." #-}

instance Core.FromJSON WebACL where
  parseJSON =
    Core.withObject "WebACL" Core.$
      \x ->
        WebACL'
          Core.<$> (x Core..: "WebACLId")
          Core.<*> (x Core..: "DefaultAction")
          Core.<*> (x Core..:? "Rules" Core..!= Core.mempty)
          Core.<*> (x Core..:? "MetricName")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "WebACLArn")
