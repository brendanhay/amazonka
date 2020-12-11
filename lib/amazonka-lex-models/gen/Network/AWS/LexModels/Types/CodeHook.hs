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
import qualified Network.AWS.Prelude as Lude

-- | Specifies a Lambda function that verifies requests to a bot or fulfills the user's request to a bot..
--
-- /See:/ 'mkCodeHook' smart constructor.
data CodeHook = CodeHook'
  { uri :: Lude.Text,
    messageVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeHook' with the minimum fields required to make a request.
--
-- * 'messageVersion' - The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
-- * 'uri' - The Amazon Resource Name (ARN) of the Lambda function.
mkCodeHook ::
  -- | 'uri'
  Lude.Text ->
  -- | 'messageVersion'
  Lude.Text ->
  CodeHook
mkCodeHook pUri_ pMessageVersion_ =
  CodeHook' {uri = pUri_, messageVersion = pMessageVersion_}

-- | The Amazon Resource Name (ARN) of the Lambda function.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chUri :: Lens.Lens' CodeHook Lude.Text
chUri = Lens.lens (uri :: CodeHook -> Lude.Text) (\s a -> s {uri = a} :: CodeHook)
{-# DEPRECATED chUri "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | The version of the request-response that you want Amazon Lex to use to invoke your Lambda function. For more information, see 'using-lambda' .
--
-- /Note:/ Consider using 'messageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chMessageVersion :: Lens.Lens' CodeHook Lude.Text
chMessageVersion = Lens.lens (messageVersion :: CodeHook -> Lude.Text) (\s a -> s {messageVersion = a} :: CodeHook)
{-# DEPRECATED chMessageVersion "Use generic-lens or generic-optics with 'messageVersion' instead." #-}

instance Lude.FromJSON CodeHook where
  parseJSON =
    Lude.withObject
      "CodeHook"
      ( \x ->
          CodeHook'
            Lude.<$> (x Lude..: "uri") Lude.<*> (x Lude..: "messageVersion")
      )

instance Lude.ToJSON CodeHook where
  toJSON CodeHook' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("uri" Lude..= uri),
            Lude.Just ("messageVersion" Lude..= messageVersion)
          ]
      )
