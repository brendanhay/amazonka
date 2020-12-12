{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RetentionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RetentionConfiguration
  ( RetentionConfiguration (..),

    -- * Smart constructor
    mkRetentionConfiguration,

    -- * Lenses
    rcName,
    rcRetentionPeriodInDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object with the name of the retention configuration and the retention period in days. The object stores the configuration for data retention in AWS Config.
--
-- /See:/ 'mkRetentionConfiguration' smart constructor.
data RetentionConfiguration = RetentionConfiguration'
  { name ::
      Lude.Text,
    retentionPeriodInDays :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RetentionConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name of the retention configuration object.
-- * 'retentionPeriodInDays' - Number of days AWS Config stores your historical information.
mkRetentionConfiguration ::
  -- | 'name'
  Lude.Text ->
  -- | 'retentionPeriodInDays'
  Lude.Natural ->
  RetentionConfiguration
mkRetentionConfiguration pName_ pRetentionPeriodInDays_ =
  RetentionConfiguration'
    { name = pName_,
      retentionPeriodInDays = pRetentionPeriodInDays_
    }

-- | The name of the retention configuration object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcName :: Lens.Lens' RetentionConfiguration Lude.Text
rcName = Lens.lens (name :: RetentionConfiguration -> Lude.Text) (\s a -> s {name = a} :: RetentionConfiguration)
{-# DEPRECATED rcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Number of days AWS Config stores your historical information.
--
-- /Note:/ Consider using 'retentionPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRetentionPeriodInDays :: Lens.Lens' RetentionConfiguration Lude.Natural
rcRetentionPeriodInDays = Lens.lens (retentionPeriodInDays :: RetentionConfiguration -> Lude.Natural) (\s a -> s {retentionPeriodInDays = a} :: RetentionConfiguration)
{-# DEPRECATED rcRetentionPeriodInDays "Use generic-lens or generic-optics with 'retentionPeriodInDays' instead." #-}

instance Lude.FromJSON RetentionConfiguration where
  parseJSON =
    Lude.withObject
      "RetentionConfiguration"
      ( \x ->
          RetentionConfiguration'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "RetentionPeriodInDays")
      )
