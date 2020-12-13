{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.AWSSessionCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.AWSSessionCredentials
  ( AWSSessionCredentials (..),

    -- * Smart constructor
    mkAWSSessionCredentials,

    -- * Lenses
    ascSecretAccessKey,
    ascSessionToken,
    ascAccessKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the S3 bucket used to store artifact for the pipeline in AWS CodePipeline.
--
-- /See:/ 'mkAWSSessionCredentials' smart constructor.
data AWSSessionCredentials = AWSSessionCredentials'
  { -- | The secret access key for the session.
    secretAccessKey :: Lude.Text,
    -- | The token for the session.
    sessionToken :: Lude.Text,
    -- | The access key for the session.
    accessKeyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSSessionCredentials' with the minimum fields required to make a request.
--
-- * 'secretAccessKey' - The secret access key for the session.
-- * 'sessionToken' - The token for the session.
-- * 'accessKeyId' - The access key for the session.
mkAWSSessionCredentials ::
  -- | 'secretAccessKey'
  Lude.Text ->
  -- | 'sessionToken'
  Lude.Text ->
  -- | 'accessKeyId'
  Lude.Text ->
  AWSSessionCredentials
mkAWSSessionCredentials
  pSecretAccessKey_
  pSessionToken_
  pAccessKeyId_ =
    AWSSessionCredentials'
      { secretAccessKey = pSecretAccessKey_,
        sessionToken = pSessionToken_,
        accessKeyId = pAccessKeyId_
      }

-- | The secret access key for the session.
--
-- /Note:/ Consider using 'secretAccessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascSecretAccessKey :: Lens.Lens' AWSSessionCredentials Lude.Text
ascSecretAccessKey = Lens.lens (secretAccessKey :: AWSSessionCredentials -> Lude.Text) (\s a -> s {secretAccessKey = a} :: AWSSessionCredentials)
{-# DEPRECATED ascSecretAccessKey "Use generic-lens or generic-optics with 'secretAccessKey' instead." #-}

-- | The token for the session.
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascSessionToken :: Lens.Lens' AWSSessionCredentials Lude.Text
ascSessionToken = Lens.lens (sessionToken :: AWSSessionCredentials -> Lude.Text) (\s a -> s {sessionToken = a} :: AWSSessionCredentials)
{-# DEPRECATED ascSessionToken "Use generic-lens or generic-optics with 'sessionToken' instead." #-}

-- | The access key for the session.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascAccessKeyId :: Lens.Lens' AWSSessionCredentials Lude.Text
ascAccessKeyId = Lens.lens (accessKeyId :: AWSSessionCredentials -> Lude.Text) (\s a -> s {accessKeyId = a} :: AWSSessionCredentials)
{-# DEPRECATED ascAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.FromJSON AWSSessionCredentials where
  parseJSON =
    Lude.withObject
      "AWSSessionCredentials"
      ( \x ->
          AWSSessionCredentials'
            Lude.<$> (x Lude..: "secretAccessKey")
            Lude.<*> (x Lude..: "sessionToken")
            Lude.<*> (x Lude..: "accessKeyId")
      )
