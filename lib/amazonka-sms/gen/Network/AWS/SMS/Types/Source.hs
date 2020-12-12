{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Source
  ( Source (..),

    -- * Smart constructor
    mkSource,

    -- * Lenses
    sS3Location,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.S3Location

-- | Contains the location of a validation script.
--
-- /See:/ 'mkSource' smart constructor.
newtype Source = Source' {s3Location :: Lude.Maybe S3Location}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- * 's3Location' - Undocumented field.
mkSource ::
  Source
mkSource = Source' {s3Location = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3Location :: Lens.Lens' Source (Lude.Maybe S3Location)
sS3Location = Lens.lens (s3Location :: Source -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: Source)
{-# DEPRECATED sS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Lude.FromJSON Source where
  parseJSON =
    Lude.withObject
      "Source"
      (\x -> Source' Lude.<$> (x Lude..:? "s3Location"))

instance Lude.ToJSON Source where
  toJSON Source' {..} =
    Lude.object
      (Lude.catMaybes [("s3Location" Lude..=) Lude.<$> s3Location])
