{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigning
  ( CodeSigning (..),

    -- * Smart constructor
    mkCodeSigning,

    -- * Lenses
    csCustomCodeSigning,
    csStartSigningJobParameter,
    csAwsSignerJobId,
  )
where

import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.StartSigningJobParameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the method to use when code signing a file.
--
-- /See:/ 'mkCodeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { customCodeSigning ::
      Lude.Maybe CustomCodeSigning,
    startSigningJobParameter :: Lude.Maybe StartSigningJobParameter,
    awsSignerJobId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeSigning' with the minimum fields required to make a request.
--
-- * 'awsSignerJobId' - The ID of the AWSSignerJob which was created to sign the file.
-- * 'customCodeSigning' - A custom method for code signing a file.
-- * 'startSigningJobParameter' - Describes the code-signing job.
mkCodeSigning ::
  CodeSigning
mkCodeSigning =
  CodeSigning'
    { customCodeSigning = Lude.Nothing,
      startSigningJobParameter = Lude.Nothing,
      awsSignerJobId = Lude.Nothing
    }

-- | A custom method for code signing a file.
--
-- /Note:/ Consider using 'customCodeSigning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCustomCodeSigning :: Lens.Lens' CodeSigning (Lude.Maybe CustomCodeSigning)
csCustomCodeSigning = Lens.lens (customCodeSigning :: CodeSigning -> Lude.Maybe CustomCodeSigning) (\s a -> s {customCodeSigning = a} :: CodeSigning)
{-# DEPRECATED csCustomCodeSigning "Use generic-lens or generic-optics with 'customCodeSigning' instead." #-}

-- | Describes the code-signing job.
--
-- /Note:/ Consider using 'startSigningJobParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStartSigningJobParameter :: Lens.Lens' CodeSigning (Lude.Maybe StartSigningJobParameter)
csStartSigningJobParameter = Lens.lens (startSigningJobParameter :: CodeSigning -> Lude.Maybe StartSigningJobParameter) (\s a -> s {startSigningJobParameter = a} :: CodeSigning)
{-# DEPRECATED csStartSigningJobParameter "Use generic-lens or generic-optics with 'startSigningJobParameter' instead." #-}

-- | The ID of the AWSSignerJob which was created to sign the file.
--
-- /Note:/ Consider using 'awsSignerJobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAwsSignerJobId :: Lens.Lens' CodeSigning (Lude.Maybe Lude.Text)
csAwsSignerJobId = Lens.lens (awsSignerJobId :: CodeSigning -> Lude.Maybe Lude.Text) (\s a -> s {awsSignerJobId = a} :: CodeSigning)
{-# DEPRECATED csAwsSignerJobId "Use generic-lens or generic-optics with 'awsSignerJobId' instead." #-}

instance Lude.FromJSON CodeSigning where
  parseJSON =
    Lude.withObject
      "CodeSigning"
      ( \x ->
          CodeSigning'
            Lude.<$> (x Lude..:? "customCodeSigning")
            Lude.<*> (x Lude..:? "startSigningJobParameter")
            Lude.<*> (x Lude..:? "awsSignerJobId")
      )

instance Lude.ToJSON CodeSigning where
  toJSON CodeSigning' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customCodeSigning" Lude..=) Lude.<$> customCodeSigning,
            ("startSigningJobParameter" Lude..=)
              Lude.<$> startSigningJobParameter,
            ("awsSignerJobId" Lude..=) Lude.<$> awsSignerJobId
          ]
      )
