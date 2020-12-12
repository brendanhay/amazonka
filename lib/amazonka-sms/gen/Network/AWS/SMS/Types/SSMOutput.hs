{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.SSMOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMOutput
  ( SSMOutput (..),

    -- * Smart constructor
    mkSSMOutput,

    -- * Lenses
    ssmoS3Location,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.S3Location

-- | Contains the location of validation output.
--
-- /See:/ 'mkSSMOutput' smart constructor.
newtype SSMOutput = SSMOutput' {s3Location :: Lude.Maybe S3Location}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSMOutput' with the minimum fields required to make a request.
--
-- * 's3Location' - Undocumented field.
mkSSMOutput ::
  SSMOutput
mkSSMOutput = SSMOutput' {s3Location = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmoS3Location :: Lens.Lens' SSMOutput (Lude.Maybe S3Location)
ssmoS3Location = Lens.lens (s3Location :: SSMOutput -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: SSMOutput)
{-# DEPRECATED ssmoS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Lude.FromJSON SSMOutput where
  parseJSON =
    Lude.withObject
      "SSMOutput"
      (\x -> SSMOutput' Lude.<$> (x Lude..:? "s3Location"))
