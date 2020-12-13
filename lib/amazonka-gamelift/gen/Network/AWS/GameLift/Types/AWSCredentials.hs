{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AWSCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.AWSCredentials
  ( AWSCredentials (..),

    -- * Smart constructor
    mkAWSCredentials,

    -- * Lenses
    acSecretAccessKey,
    acSessionToken,
    acAccessKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Temporary access credentials used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling 'RequestUploadCredentials' .
--
-- /See:/ 'mkAWSCredentials' smart constructor.
data AWSCredentials = AWSCredentials'
  { -- | Temporary secret key allowing access to the Amazon GameLift S3 account.
    secretAccessKey :: Lude.Maybe Lude.Text,
    -- | Token used to associate a specific build ID with the files uploaded using these credentials.
    sessionToken :: Lude.Maybe Lude.Text,
    -- | Temporary key allowing access to the Amazon GameLift S3 account.
    accessKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AWSCredentials' with the minimum fields required to make a request.
--
-- * 'secretAccessKey' - Temporary secret key allowing access to the Amazon GameLift S3 account.
-- * 'sessionToken' - Token used to associate a specific build ID with the files uploaded using these credentials.
-- * 'accessKeyId' - Temporary key allowing access to the Amazon GameLift S3 account.
mkAWSCredentials ::
  AWSCredentials
mkAWSCredentials =
  AWSCredentials'
    { secretAccessKey = Lude.Nothing,
      sessionToken = Lude.Nothing,
      accessKeyId = Lude.Nothing
    }

-- | Temporary secret key allowing access to the Amazon GameLift S3 account.
--
-- /Note:/ Consider using 'secretAccessKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSecretAccessKey :: Lens.Lens' AWSCredentials (Lude.Maybe Lude.Text)
acSecretAccessKey = Lens.lens (secretAccessKey :: AWSCredentials -> Lude.Maybe Lude.Text) (\s a -> s {secretAccessKey = a} :: AWSCredentials)
{-# DEPRECATED acSecretAccessKey "Use generic-lens or generic-optics with 'secretAccessKey' instead." #-}

-- | Token used to associate a specific build ID with the files uploaded using these credentials.
--
-- /Note:/ Consider using 'sessionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acSessionToken :: Lens.Lens' AWSCredentials (Lude.Maybe Lude.Text)
acSessionToken = Lens.lens (sessionToken :: AWSCredentials -> Lude.Maybe Lude.Text) (\s a -> s {sessionToken = a} :: AWSCredentials)
{-# DEPRECATED acSessionToken "Use generic-lens or generic-optics with 'sessionToken' instead." #-}

-- | Temporary key allowing access to the Amazon GameLift S3 account.
--
-- /Note:/ Consider using 'accessKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAccessKeyId :: Lens.Lens' AWSCredentials (Lude.Maybe Lude.Text)
acAccessKeyId = Lens.lens (accessKeyId :: AWSCredentials -> Lude.Maybe Lude.Text) (\s a -> s {accessKeyId = a} :: AWSCredentials)
{-# DEPRECATED acAccessKeyId "Use generic-lens or generic-optics with 'accessKeyId' instead." #-}

instance Lude.FromJSON AWSCredentials where
  parseJSON =
    Lude.withObject
      "AWSCredentials"
      ( \x ->
          AWSCredentials'
            Lude.<$> (x Lude..:? "SecretAccessKey")
            Lude.<*> (x Lude..:? "SessionToken")
            Lude.<*> (x Lude..:? "AccessKeyId")
      )
