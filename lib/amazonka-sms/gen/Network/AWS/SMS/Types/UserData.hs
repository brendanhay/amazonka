{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.UserData
  ( UserData (..),

    -- * Smart constructor
    mkUserData,

    -- * Lenses
    udS3Location,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.S3Location

-- | A script that runs on first launch of an Amazon EC2 instance. Used for configuring the server during launch.
--
-- /See:/ 'mkUserData' smart constructor.
newtype UserData = UserData' {s3Location :: Lude.Maybe S3Location}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- * 's3Location' - Amazon S3 location of the user-data script.
mkUserData ::
  UserData
mkUserData = UserData' {s3Location = Lude.Nothing}

-- | Amazon S3 location of the user-data script.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udS3Location :: Lens.Lens' UserData (Lude.Maybe S3Location)
udS3Location = Lens.lens (s3Location :: UserData -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: UserData)
{-# DEPRECATED udS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Lude.FromJSON UserData where
  parseJSON =
    Lude.withObject
      "UserData"
      (\x -> UserData' Lude.<$> (x Lude..:? "s3Location"))

instance Lude.ToJSON UserData where
  toJSON UserData' {..} =
    Lude.object
      (Lude.catMaybes [("s3Location" Lude..=) Lude.<$> s3Location])
