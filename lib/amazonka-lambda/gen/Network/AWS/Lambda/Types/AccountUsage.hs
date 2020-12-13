{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AccountUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AccountUsage
  ( AccountUsage (..),

    -- * Smart constructor
    mkAccountUsage,

    -- * Lenses
    auTotalCodeSize,
    auFunctionCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of functions and amount of storage in use.
--
-- /See:/ 'mkAccountUsage' smart constructor.
data AccountUsage = AccountUsage'
  { -- | The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
    totalCodeSize :: Lude.Maybe Lude.Integer,
    -- | The number of Lambda functions.
    functionCount :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccountUsage' with the minimum fields required to make a request.
--
-- * 'totalCodeSize' - The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
-- * 'functionCount' - The number of Lambda functions.
mkAccountUsage ::
  AccountUsage
mkAccountUsage =
  AccountUsage'
    { totalCodeSize = Lude.Nothing,
      functionCount = Lude.Nothing
    }

-- | The amount of storage space, in bytes, that's being used by deployment packages and layer archives.
--
-- /Note:/ Consider using 'totalCodeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auTotalCodeSize :: Lens.Lens' AccountUsage (Lude.Maybe Lude.Integer)
auTotalCodeSize = Lens.lens (totalCodeSize :: AccountUsage -> Lude.Maybe Lude.Integer) (\s a -> s {totalCodeSize = a} :: AccountUsage)
{-# DEPRECATED auTotalCodeSize "Use generic-lens or generic-optics with 'totalCodeSize' instead." #-}

-- | The number of Lambda functions.
--
-- /Note:/ Consider using 'functionCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auFunctionCount :: Lens.Lens' AccountUsage (Lude.Maybe Lude.Integer)
auFunctionCount = Lens.lens (functionCount :: AccountUsage -> Lude.Maybe Lude.Integer) (\s a -> s {functionCount = a} :: AccountUsage)
{-# DEPRECATED auFunctionCount "Use generic-lens or generic-optics with 'functionCount' instead." #-}

instance Lude.FromJSON AccountUsage where
  parseJSON =
    Lude.withObject
      "AccountUsage"
      ( \x ->
          AccountUsage'
            Lude.<$> (x Lude..:? "TotalCodeSize") Lude.<*> (x Lude..:? "FunctionCount")
      )
