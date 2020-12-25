{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.CodeHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.CodeHook
  ( CodeHook (..),

    -- * Smart constructor
    mkCodeHook,

    -- * Lenses
    chUri,
    chMessageVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.LambdaARN as Types
import qualified Network.AWS.LexModels.Types.MessageVersion as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies a Lambda function that verifies requests to a bot or fulfills the user's request to a bot..
--
-- /See:/ 'mkCodeHook' smart constructor.
data CodeHook = CodeHook'
  { -- | The Amazon Resource Name (ARN) of the Lambda function.
    uri :: Types.LambdaARN,
    -- | The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
    messageVersion :: Types.MessageVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeHook' value with any optional fields omitted.
mkCodeHook ::
  -- | 'uri'
  Types.LambdaARN ->
  -- | 'messageVersion'
  Types.MessageVersion ->
  CodeHook
mkCodeHook uri messageVersion = CodeHook' {uri, messageVersion}

-- | The Amazon Resource Name (ARN) of the Lambda function.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chUri :: Lens.Lens' CodeHook Types.LambdaARN
chUri = Lens.field @"uri"
{-# DEPRECATED chUri "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
--
-- /Note:/ Consider using 'messageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chMessageVersion :: Lens.Lens' CodeHook Types.MessageVersion
chMessageVersion = Lens.field @"messageVersion"
{-# DEPRECATED chMessageVersion "Use generic-lens or generic-optics with 'messageVersion' instead." #-}

instance Core.FromJSON CodeHook where
  toJSON CodeHook {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("uri" Core..= uri),
            Core.Just ("messageVersion" Core..= messageVersion)
          ]
      )

instance Core.FromJSON CodeHook where
  parseJSON =
    Core.withObject "CodeHook" Core.$
      \x ->
        CodeHook'
          Core.<$> (x Core..: "uri") Core.<*> (x Core..: "messageVersion")
