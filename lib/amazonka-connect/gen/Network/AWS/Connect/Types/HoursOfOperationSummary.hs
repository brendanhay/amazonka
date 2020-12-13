{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HoursOfOperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HoursOfOperationSummary
  ( HoursOfOperationSummary (..),

    -- * Smart constructor
    mkHoursOfOperationSummary,

    -- * Lenses
    hoosARN,
    hoosName,
    hoosId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about hours of operation for a contact center.
--
-- /See:/ 'mkHoursOfOperationSummary' smart constructor.
data HoursOfOperationSummary = HoursOfOperationSummary'
  { -- | The Amazon Resource Name (ARN) of the hours of operation.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the hours of operation.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the hours of operation.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HoursOfOperationSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the hours of operation.
-- * 'name' - The name of the hours of operation.
-- * 'id' - The identifier of the hours of operation.
mkHoursOfOperationSummary ::
  HoursOfOperationSummary
mkHoursOfOperationSummary =
  HoursOfOperationSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hours of operation.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoosARN :: Lens.Lens' HoursOfOperationSummary (Lude.Maybe Lude.Text)
hoosARN = Lens.lens (arn :: HoursOfOperationSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: HoursOfOperationSummary)
{-# DEPRECATED hoosARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the hours of operation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoosName :: Lens.Lens' HoursOfOperationSummary (Lude.Maybe Lude.Text)
hoosName = Lens.lens (name :: HoursOfOperationSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HoursOfOperationSummary)
{-# DEPRECATED hoosName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the hours of operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoosId :: Lens.Lens' HoursOfOperationSummary (Lude.Maybe Lude.Text)
hoosId = Lens.lens (id :: HoursOfOperationSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: HoursOfOperationSummary)
{-# DEPRECATED hoosId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON HoursOfOperationSummary where
  parseJSON =
    Lude.withObject
      "HoursOfOperationSummary"
      ( \x ->
          HoursOfOperationSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
