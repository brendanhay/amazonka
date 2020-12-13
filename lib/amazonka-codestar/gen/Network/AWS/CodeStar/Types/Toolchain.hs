{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Toolchain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Toolchain
  ( Toolchain (..),

    -- * Smart constructor
    mkToolchain,

    -- * Lenses
    tStackParameters,
    tSource,
    tRoleARN,
  )
where

import Network.AWS.CodeStar.Types.ToolchainSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The toolchain template file provided with the project request. AWS CodeStar uses the template to provision the toolchain stack in AWS CloudFormation.
--
-- /See:/ 'mkToolchain' smart constructor.
data Toolchain = Toolchain'
  { -- | The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
    stackParameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)),
    -- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
    source :: ToolchainSource,
    -- | The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Toolchain' with the minimum fields required to make a request.
--
-- * 'stackParameters' - The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
-- * 'source' - The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
-- * 'roleARN' - The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
mkToolchain ::
  -- | 'source'
  ToolchainSource ->
  Toolchain
mkToolchain pSource_ =
  Toolchain'
    { stackParameters = Lude.Nothing,
      source = pSource_,
      roleARN = Lude.Nothing
    }

-- | The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
--
-- /Note:/ Consider using 'stackParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStackParameters :: Lens.Lens' Toolchain (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)))
tStackParameters = Lens.lens (stackParameters :: Toolchain -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text))) (\s a -> s {stackParameters = a} :: Toolchain)
{-# DEPRECATED tStackParameters "Use generic-lens or generic-optics with 'stackParameters' instead." #-}

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSource :: Lens.Lens' Toolchain ToolchainSource
tSource = Lens.lens (source :: Toolchain -> ToolchainSource) (\s a -> s {source = a} :: Toolchain)
{-# DEPRECATED tSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRoleARN :: Lens.Lens' Toolchain (Lude.Maybe Lude.Text)
tRoleARN = Lens.lens (roleARN :: Toolchain -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Toolchain)
{-# DEPRECATED tRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON Toolchain where
  toJSON Toolchain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stackParameters" Lude..=) Lude.<$> stackParameters,
            Lude.Just ("source" Lude..= source),
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )
