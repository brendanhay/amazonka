{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PutItemInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutItemInput
  ( PutItemInput (..),

    -- * Smart constructor
    mkPutItemInput,

    -- * Lenses
    piiTableName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The input for the DynamoActionVS action that specifies the DynamoDB table to which the message data will be written.
--
-- /See:/ 'mkPutItemInput' smart constructor.
newtype PutItemInput = PutItemInput'
  { -- | The table where the message data will be written.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutItemInput' with the minimum fields required to make a request.
--
-- * 'tableName' - The table where the message data will be written.
mkPutItemInput ::
  -- | 'tableName'
  Lude.Text ->
  PutItemInput
mkPutItemInput pTableName_ = PutItemInput' {tableName = pTableName_}

-- | The table where the message data will be written.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piiTableName :: Lens.Lens' PutItemInput Lude.Text
piiTableName = Lens.lens (tableName :: PutItemInput -> Lude.Text) (\s a -> s {tableName = a} :: PutItemInput)
{-# DEPRECATED piiTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON PutItemInput where
  parseJSON =
    Lude.withObject
      "PutItemInput"
      (\x -> PutItemInput' Lude.<$> (x Lude..: "tableName"))

instance Lude.ToJSON PutItemInput where
  toJSON PutItemInput' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("tableName" Lude..= tableName)])
