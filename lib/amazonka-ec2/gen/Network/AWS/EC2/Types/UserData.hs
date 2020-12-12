{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserData
  ( UserData (..),

    -- * Smart constructor
    mkUserData,

    -- * Lenses
    udData,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the user data for an instance.
--
-- /See:/ 'mkUserData' smart constructor.
newtype UserData = UserData' {data' :: Lude.Maybe Lude.Text}
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
-- * 'data'' - The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
mkUserData ::
  UserData
mkUserData = UserData' {data' = Lude.Nothing}

-- | The user data. If you are using an AWS SDK or command line tool, Base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide Base64-encoded text.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udData :: Lens.Lens' UserData (Lude.Maybe Lude.Text)
udData = Lens.lens (data' :: UserData -> Lude.Maybe Lude.Text) (\s a -> s {data' = a} :: UserData)
{-# DEPRECATED udData "Use generic-lens or generic-optics with 'data'' instead." #-}

instance Lude.ToQuery UserData where
  toQuery UserData' {..} = Lude.mconcat ["Data" Lude.=: data']
