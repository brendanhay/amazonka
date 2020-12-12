{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.SupportService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.SupportService
  ( SupportService (..),

    -- * Smart constructor
    mkSupportService,

    -- * Lenses
    ssCategories,
    ssName,
    ssCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Support.Types.Category

-- | Information about an AWS service returned by the 'DescribeServices' operation.
--
-- /See:/ 'mkSupportService' smart constructor.
data SupportService = SupportService'
  { categories ::
      Lude.Maybe [Category],
    name :: Lude.Maybe Lude.Text,
    code :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SupportService' with the minimum fields required to make a request.
--
-- * 'categories' - A list of categories that describe the type of support issue a case describes. Categories consist of a category name and a category code. Category names and codes are passed to AWS Support when you call 'CreateCase' .
-- * 'code' - The code for an AWS service returned by the 'DescribeServices' response. The @name@ element contains the corresponding friendly name.
-- * 'name' - The friendly name for an AWS service. The @code@ element contains the corresponding code.
mkSupportService ::
  SupportService
mkSupportService =
  SupportService'
    { categories = Lude.Nothing,
      name = Lude.Nothing,
      code = Lude.Nothing
    }

-- | A list of categories that describe the type of support issue a case describes. Categories consist of a category name and a category code. Category names and codes are passed to AWS Support when you call 'CreateCase' .
--
-- /Note:/ Consider using 'categories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCategories :: Lens.Lens' SupportService (Lude.Maybe [Category])
ssCategories = Lens.lens (categories :: SupportService -> Lude.Maybe [Category]) (\s a -> s {categories = a} :: SupportService)
{-# DEPRECATED ssCategories "Use generic-lens or generic-optics with 'categories' instead." #-}

-- | The friendly name for an AWS service. The @code@ element contains the corresponding code.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' SupportService (Lude.Maybe Lude.Text)
ssName = Lens.lens (name :: SupportService -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SupportService)
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The code for an AWS service returned by the 'DescribeServices' response. The @name@ element contains the corresponding friendly name.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCode :: Lens.Lens' SupportService (Lude.Maybe Lude.Text)
ssCode = Lens.lens (code :: SupportService -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: SupportService)
{-# DEPRECATED ssCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON SupportService where
  parseJSON =
    Lude.withObject
      "SupportService"
      ( \x ->
          SupportService'
            Lude.<$> (x Lude..:? "categories" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "code")
      )
