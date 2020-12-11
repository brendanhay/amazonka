{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteV2LoggingLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logging level.
module Network.AWS.IoT.DeleteV2LoggingLevel
  ( -- * Creating a request
    DeleteV2LoggingLevel (..),
    mkDeleteV2LoggingLevel,

    -- ** Request lenses
    dvllTargetType,
    dvllTargetName,

    -- * Destructuring the response
    DeleteV2LoggingLevelResponse (..),
    mkDeleteV2LoggingLevelResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteV2LoggingLevel' smart constructor.
data DeleteV2LoggingLevel = DeleteV2LoggingLevel'
  { targetType ::
      LogTargetType,
    targetName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteV2LoggingLevel' with the minimum fields required to make a request.
--
-- * 'targetName' - The name of the resource for which you are configuring logging.
-- * 'targetType' - The type of resource for which you are configuring logging. Must be @THING_Group@ .
mkDeleteV2LoggingLevel ::
  -- | 'targetType'
  LogTargetType ->
  -- | 'targetName'
  Lude.Text ->
  DeleteV2LoggingLevel
mkDeleteV2LoggingLevel pTargetType_ pTargetName_ =
  DeleteV2LoggingLevel'
    { targetType = pTargetType_,
      targetName = pTargetName_
    }

-- | The type of resource for which you are configuring logging. Must be @THING_Group@ .
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvllTargetType :: Lens.Lens' DeleteV2LoggingLevel LogTargetType
dvllTargetType = Lens.lens (targetType :: DeleteV2LoggingLevel -> LogTargetType) (\s a -> s {targetType = a} :: DeleteV2LoggingLevel)
{-# DEPRECATED dvllTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The name of the resource for which you are configuring logging.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvllTargetName :: Lens.Lens' DeleteV2LoggingLevel Lude.Text
dvllTargetName = Lens.lens (targetName :: DeleteV2LoggingLevel -> Lude.Text) (\s a -> s {targetName = a} :: DeleteV2LoggingLevel)
{-# DEPRECATED dvllTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

instance Lude.AWSRequest DeleteV2LoggingLevel where
  type Rs DeleteV2LoggingLevel = DeleteV2LoggingLevelResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeleteV2LoggingLevelResponse'

instance Lude.ToHeaders DeleteV2LoggingLevel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteV2LoggingLevel where
  toPath = Lude.const "/v2LoggingLevel"

instance Lude.ToQuery DeleteV2LoggingLevel where
  toQuery DeleteV2LoggingLevel' {..} =
    Lude.mconcat
      ["targetType" Lude.=: targetType, "targetName" Lude.=: targetName]

-- | /See:/ 'mkDeleteV2LoggingLevelResponse' smart constructor.
data DeleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteV2LoggingLevelResponse' with the minimum fields required to make a request.
mkDeleteV2LoggingLevelResponse ::
  DeleteV2LoggingLevelResponse
mkDeleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'
