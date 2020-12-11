-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SourceIPConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceIPConfig
  ( SourceIPConfig (..),

    -- * Smart constructor
    mkSourceIPConfig,

    -- * Lenses
    sicCidrs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of IP address ranges (<https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html CIDRs> ). Used to create an allow list of IP addresses for a private workforce. Workers will only be able to login to their worker portal from an IP address within this range. By default, a workforce isn't restricted to specific IP addresses.
--
-- /See:/ 'mkSourceIPConfig' smart constructor.
newtype SourceIPConfig = SourceIPConfig' {cidrs :: [Lude.Text]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceIPConfig' with the minimum fields required to make a request.
--
-- * 'cidrs' - A list of one to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing> (CIDR) values.
--
-- Maximum: Ten CIDR values
mkSourceIPConfig ::
  SourceIPConfig
mkSourceIPConfig = SourceIPConfig' {cidrs = Lude.mempty}

-- | A list of one to ten <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Classless Inter-Domain Routing> (CIDR) values.
--
-- Maximum: Ten CIDR values
--
-- /Note:/ Consider using 'cidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicCidrs :: Lens.Lens' SourceIPConfig [Lude.Text]
sicCidrs = Lens.lens (cidrs :: SourceIPConfig -> [Lude.Text]) (\s a -> s {cidrs = a} :: SourceIPConfig)
{-# DEPRECATED sicCidrs "Use generic-lens or generic-optics with 'cidrs' instead." #-}

instance Lude.FromJSON SourceIPConfig where
  parseJSON =
    Lude.withObject
      "SourceIPConfig"
      ( \x ->
          SourceIPConfig' Lude.<$> (x Lude..:? "Cidrs" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SourceIPConfig where
  toJSON SourceIPConfig' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Cidrs" Lude..= cidrs)])
