{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicy
  ( TrafficPolicy (..),

    -- * Smart constructor
    mkTrafficPolicy,

    -- * Lenses
    tpDocument,
    tpName,
    tpVersion,
    tpId,
    tpType,
    tpComment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.RecordType

-- | A complex type that contains settings for a traffic policy.
--
-- /See:/ 'mkTrafficPolicy' smart constructor.
data TrafficPolicy = TrafficPolicy'
  { -- | The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
    document :: Lude.Text,
    -- | The name that you specified when you created the traffic policy.
    name :: Lude.Text,
    -- | The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
    version :: Lude.Natural,
    -- | The ID that Amazon Route 53 assigned to a traffic policy when you created it.
    id :: Lude.Text,
    -- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
    type' :: RecordType,
    -- | The comment that you specify in the @CreateTrafficPolicy@ request, if any.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficPolicy' with the minimum fields required to make a request.
--
-- * 'document' - The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
-- * 'name' - The name that you specified when you created the traffic policy.
-- * 'version' - The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
-- * 'id' - The ID that Amazon Route 53 assigned to a traffic policy when you created it.
-- * 'type'' - The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
-- * 'comment' - The comment that you specify in the @CreateTrafficPolicy@ request, if any.
mkTrafficPolicy ::
  -- | 'document'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Natural ->
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  RecordType ->
  TrafficPolicy
mkTrafficPolicy pDocument_ pName_ pVersion_ pId_ pType_ =
  TrafficPolicy'
    { document = pDocument_,
      name = pName_,
      version = pVersion_,
      id = pId_,
      type' = pType_,
      comment = Lude.Nothing
    }

-- | The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <https://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpDocument :: Lens.Lens' TrafficPolicy Lude.Text
tpDocument = Lens.lens (document :: TrafficPolicy -> Lude.Text) (\s a -> s {document = a} :: TrafficPolicy)
{-# DEPRECATED tpDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The name that you specified when you created the traffic policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpName :: Lens.Lens' TrafficPolicy Lude.Text
tpName = Lens.lens (name :: TrafficPolicy -> Lude.Text) (\s a -> s {name = a} :: TrafficPolicy)
{-# DEPRECATED tpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpVersion :: Lens.Lens' TrafficPolicy Lude.Natural
tpVersion = Lens.lens (version :: TrafficPolicy -> Lude.Natural) (\s a -> s {version = a} :: TrafficPolicy)
{-# DEPRECATED tpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID that Amazon Route 53 assigned to a traffic policy when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpId :: Lens.Lens' TrafficPolicy Lude.Text
tpId = Lens.lens (id :: TrafficPolicy -> Lude.Text) (\s a -> s {id = a} :: TrafficPolicy)
{-# DEPRECATED tpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpType :: Lens.Lens' TrafficPolicy RecordType
tpType = Lens.lens (type' :: TrafficPolicy -> RecordType) (\s a -> s {type' = a} :: TrafficPolicy)
{-# DEPRECATED tpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The comment that you specify in the @CreateTrafficPolicy@ request, if any.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpComment :: Lens.Lens' TrafficPolicy (Lude.Maybe Lude.Text)
tpComment = Lens.lens (comment :: TrafficPolicy -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: TrafficPolicy)
{-# DEPRECATED tpComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.FromXML TrafficPolicy where
  parseXML x =
    TrafficPolicy'
      Lude.<$> (x Lude..@ "Document")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "Version")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "Type")
      Lude.<*> (x Lude..@? "Comment")
