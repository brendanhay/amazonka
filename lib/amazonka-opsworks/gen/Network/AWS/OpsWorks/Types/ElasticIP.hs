-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ElasticIP
  ( ElasticIP (..),

    -- * Smart constructor
    mkElasticIP,

    -- * Lenses
    eiInstanceId,
    eiDomain,
    eiIP,
    eiName,
    eiRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Elastic IP address.
--
-- /See:/ 'mkElasticIP' smart constructor.
data ElasticIP = ElasticIP'
  { instanceId :: Lude.Maybe Lude.Text,
    domain :: Lude.Maybe Lude.Text,
    ip :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticIP' with the minimum fields required to make a request.
--
-- * 'domain' - The domain.
-- * 'instanceId' - The ID of the instance that the address is attached to.
-- * 'ip' - The IP address.
-- * 'name' - The name.
-- * 'region' - The AWS region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
mkElasticIP ::
  ElasticIP
mkElasticIP =
  ElasticIP'
    { instanceId = Lude.Nothing,
      domain = Lude.Nothing,
      ip = Lude.Nothing,
      name = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The ID of the instance that the address is attached to.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiInstanceId :: Lens.Lens' ElasticIP (Lude.Maybe Lude.Text)
eiInstanceId = Lens.lens (instanceId :: ElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ElasticIP)
{-# DEPRECATED eiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The domain.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiDomain :: Lens.Lens' ElasticIP (Lude.Maybe Lude.Text)
eiDomain = Lens.lens (domain :: ElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: ElasticIP)
{-# DEPRECATED eiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The IP address.
--
-- /Note:/ Consider using 'ip' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiIP :: Lens.Lens' ElasticIP (Lude.Maybe Lude.Text)
eiIP = Lens.lens (ip :: ElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {ip = a} :: ElasticIP)
{-# DEPRECATED eiIP "Use generic-lens or generic-optics with 'ip' instead." #-}

-- | The name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiName :: Lens.Lens' ElasticIP (Lude.Maybe Lude.Text)
eiName = Lens.lens (name :: ElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ElasticIP)
{-# DEPRECATED eiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiRegion :: Lens.Lens' ElasticIP (Lude.Maybe Lude.Text)
eiRegion = Lens.lens (region :: ElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ElasticIP)
{-# DEPRECATED eiRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON ElasticIP where
  parseJSON =
    Lude.withObject
      "ElasticIP"
      ( \x ->
          ElasticIP'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Domain")
            Lude.<*> (x Lude..:? "Ip")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Region")
      )
