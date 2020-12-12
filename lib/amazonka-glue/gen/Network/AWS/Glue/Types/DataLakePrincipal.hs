{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataLakePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataLakePrincipal
  ( DataLakePrincipal (..),

    -- * Smart constructor
    mkDataLakePrincipal,

    -- * Lenses
    dlpDataLakePrincipalIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS Lake Formation principal.
--
-- /See:/ 'mkDataLakePrincipal' smart constructor.
newtype DataLakePrincipal = DataLakePrincipal'
  { dataLakePrincipalIdentifier ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataLakePrincipal' with the minimum fields required to make a request.
--
-- * 'dataLakePrincipalIdentifier' - An identifier for the AWS Lake Formation principal.
mkDataLakePrincipal ::
  DataLakePrincipal
mkDataLakePrincipal =
  DataLakePrincipal' {dataLakePrincipalIdentifier = Lude.Nothing}

-- | An identifier for the AWS Lake Formation principal.
--
-- /Note:/ Consider using 'dataLakePrincipalIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpDataLakePrincipalIdentifier :: Lens.Lens' DataLakePrincipal (Lude.Maybe Lude.Text)
dlpDataLakePrincipalIdentifier = Lens.lens (dataLakePrincipalIdentifier :: DataLakePrincipal -> Lude.Maybe Lude.Text) (\s a -> s {dataLakePrincipalIdentifier = a} :: DataLakePrincipal)
{-# DEPRECATED dlpDataLakePrincipalIdentifier "Use generic-lens or generic-optics with 'dataLakePrincipalIdentifier' instead." #-}

instance Lude.FromJSON DataLakePrincipal where
  parseJSON =
    Lude.withObject
      "DataLakePrincipal"
      ( \x ->
          DataLakePrincipal'
            Lude.<$> (x Lude..:? "DataLakePrincipalIdentifier")
      )

instance Lude.ToJSON DataLakePrincipal where
  toJSON DataLakePrincipal' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DataLakePrincipalIdentifier" Lude..=)
              Lude.<$> dataLakePrincipalIdentifier
          ]
      )
