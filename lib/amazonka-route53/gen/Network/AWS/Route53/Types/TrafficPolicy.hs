{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.TrafficPolicy
  ( TrafficPolicy (..)
  -- * Smart constructor
  , mkTrafficPolicy
  -- * Lenses
  , tpId
  , tpVersion
  , tpName
  , tpType
  , tpDocument
  , tpComment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.Comment as Types
import qualified Network.AWS.Route53.Types.Document as Types
import qualified Network.AWS.Route53.Types.Id as Types
import qualified Network.AWS.Route53.Types.Name as Types
import qualified Network.AWS.Route53.Types.RecordType as Types

-- | A complex type that contains settings for a traffic policy.
--
-- /See:/ 'mkTrafficPolicy' smart constructor.
data TrafficPolicy = TrafficPolicy'
  { id :: Types.Id
    -- ^ The ID that Amazon Route 53 assigned to a traffic policy when you created it.
  , version :: Core.Natural
    -- ^ The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
  , name :: Types.Name
    -- ^ The name that you specified when you created the traffic policy.
  , type' :: Types.RecordType
    -- ^ The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
  , document :: Types.Document
    -- ^ The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
  , comment :: Core.Maybe Types.Comment
    -- ^ The comment that you specify in the @CreateTrafficPolicy@ request, if any.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficPolicy' value with any optional fields omitted.
mkTrafficPolicy
    :: Types.Id -- ^ 'id'
    -> Core.Natural -- ^ 'version'
    -> Types.Name -- ^ 'name'
    -> Types.RecordType -- ^ 'type\''
    -> Types.Document -- ^ 'document'
    -> TrafficPolicy
mkTrafficPolicy id version name type' document
  = TrafficPolicy'{id, version, name, type', document,
                   comment = Core.Nothing}

-- | The ID that Amazon Route 53 assigned to a traffic policy when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpId :: Lens.Lens' TrafficPolicy Types.Id
tpId = Lens.field @"id"
{-# INLINEABLE tpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpVersion :: Lens.Lens' TrafficPolicy Core.Natural
tpVersion = Lens.field @"version"
{-# INLINEABLE tpVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The name that you specified when you created the traffic policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpName :: Lens.Lens' TrafficPolicy Types.Name
tpName = Lens.field @"name"
{-# INLINEABLE tpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpType :: Lens.Lens' TrafficPolicy Types.RecordType
tpType = Lens.field @"type'"
{-# INLINEABLE tpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDocument :: Lens.Lens' TrafficPolicy Types.Document
tpDocument = Lens.field @"document"
{-# INLINEABLE tpDocument #-}
{-# DEPRECATED document "Use generic-lens or generic-optics with 'document' instead"  #-}

-- | The comment that you specify in the @CreateTrafficPolicy@ request, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpComment :: Lens.Lens' TrafficPolicy (Core.Maybe Types.Comment)
tpComment = Lens.field @"comment"
{-# INLINEABLE tpComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.FromXML TrafficPolicy where
        parseXML x
          = TrafficPolicy' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "Version" Core.<*>
                x Core..@ "Name"
                Core.<*> x Core..@ "Type"
                Core.<*> x Core..@ "Document"
                Core.<*> x Core..@? "Comment"
