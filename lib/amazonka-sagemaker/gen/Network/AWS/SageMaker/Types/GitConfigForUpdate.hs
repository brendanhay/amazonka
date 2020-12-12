{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.GitConfigForUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.GitConfigForUpdate
  ( GitConfigForUpdate (..),

    -- * Smart constructor
    mkGitConfigForUpdate,

    -- * Lenses
    gcfuSecretARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies configuration details for a Git repository when the repository is updated.
--
-- /See:/ 'mkGitConfigForUpdate' smart constructor.
newtype GitConfigForUpdate = GitConfigForUpdate'
  { secretARN ::
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

-- | Creates a value of 'GitConfigForUpdate' with the minimum fields required to make a request.
--
-- * 'secretARN' - The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
mkGitConfigForUpdate ::
  GitConfigForUpdate
mkGitConfigForUpdate =
  GitConfigForUpdate' {secretARN = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the git repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
--
-- /Note:/ Consider using 'secretARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfuSecretARN :: Lens.Lens' GitConfigForUpdate (Lude.Maybe Lude.Text)
gcfuSecretARN = Lens.lens (secretARN :: GitConfigForUpdate -> Lude.Maybe Lude.Text) (\s a -> s {secretARN = a} :: GitConfigForUpdate)
{-# DEPRECATED gcfuSecretARN "Use generic-lens or generic-optics with 'secretARN' instead." #-}

instance Lude.ToJSON GitConfigForUpdate where
  toJSON GitConfigForUpdate' {..} =
    Lude.object
      (Lude.catMaybes [("SecretArn" Lude..=) Lude.<$> secretARN])
