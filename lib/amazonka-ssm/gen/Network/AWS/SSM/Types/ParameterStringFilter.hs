{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterStringFilter
  ( ParameterStringFilter (..),

    -- * Smart constructor
    mkParameterStringFilter,

    -- * Lenses
    psfValues,
    psfKey,
    psfOption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /See:/ 'mkParameterStringFilter' smart constructor.
data ParameterStringFilter = ParameterStringFilter'
  { -- | The value you want to search for.
    values :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The name of the filter.
    key :: Lude.Text,
    -- | For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .)
    --
    -- For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
    option :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterStringFilter' with the minimum fields required to make a request.
--
-- * 'values' - The value you want to search for.
-- * 'key' - The name of the filter.
-- * 'option' - For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .)
--
-- For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
mkParameterStringFilter ::
  -- | 'key'
  Lude.Text ->
  ParameterStringFilter
mkParameterStringFilter pKey_ =
  ParameterStringFilter'
    { values = Lude.Nothing,
      key = pKey_,
      option = Lude.Nothing
    }

-- | The value you want to search for.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfValues :: Lens.Lens' ParameterStringFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
psfValues = Lens.lens (values :: ParameterStringFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {values = a} :: ParameterStringFilter)
{-# DEPRECATED psfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfKey :: Lens.Lens' ParameterStringFilter Lude.Text
psfKey = Lens.lens (key :: ParameterStringFilter -> Lude.Text) (\s a -> s {key = a} :: ParameterStringFilter)
{-# DEPRECATED psfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | For all filters used with 'DescribeParameters' , valid options include @Equals@ and @BeginsWith@ . The @Name@ filter additionally supports the @Contains@ option. (Exception: For filters using the key @Path@ , valid options include @Recursive@ and @OneLevel@ .)
--
-- For filters used with 'GetParametersByPath' , valid options include @Equals@ and @BeginsWith@ . (Exception: For filters using @Label@ as the Key name, the only valid option is @Equals@ .)
--
-- /Note:/ Consider using 'option' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psfOption :: Lens.Lens' ParameterStringFilter (Lude.Maybe Lude.Text)
psfOption = Lens.lens (option :: ParameterStringFilter -> Lude.Maybe Lude.Text) (\s a -> s {option = a} :: ParameterStringFilter)
{-# DEPRECATED psfOption "Use generic-lens or generic-optics with 'option' instead." #-}

instance Lude.ToJSON ParameterStringFilter where
  toJSON ParameterStringFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            Lude.Just ("Key" Lude..= key),
            ("Option" Lude..=) Lude.<$> option
          ]
      )
