-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.LastReportGenerationExecutionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.LastReportGenerationExecutionError
  ( LastReportGenerationExecutionError (..),

    -- * Smart constructor
    mkLastReportGenerationExecutionError,

    -- * Lenses
    lrgeeErrorCode,
    lrgeeErrorMessage,
  )
where

import Network.AWS.AppStream.Types.UsageReportExecutionErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the error that is returned when a usage report can't be generated.
--
-- /See:/ 'mkLastReportGenerationExecutionError' smart constructor.
data LastReportGenerationExecutionError = LastReportGenerationExecutionError'
  { errorCode ::
      Lude.Maybe
        UsageReportExecutionErrorCode,
    errorMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LastReportGenerationExecutionError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code for the error that is returned when a usage report can't be generated.
-- * 'errorMessage' - The error message for the error that is returned when a usage report can't be generated.
mkLastReportGenerationExecutionError ::
  LastReportGenerationExecutionError
mkLastReportGenerationExecutionError =
  LastReportGenerationExecutionError'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The error code for the error that is returned when a usage report can't be generated.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgeeErrorCode :: Lens.Lens' LastReportGenerationExecutionError (Lude.Maybe UsageReportExecutionErrorCode)
lrgeeErrorCode = Lens.lens (errorCode :: LastReportGenerationExecutionError -> Lude.Maybe UsageReportExecutionErrorCode) (\s a -> s {errorCode = a} :: LastReportGenerationExecutionError)
{-# DEPRECATED lrgeeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for the error that is returned when a usage report can't be generated.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgeeErrorMessage :: Lens.Lens' LastReportGenerationExecutionError (Lude.Maybe Lude.Text)
lrgeeErrorMessage = Lens.lens (errorMessage :: LastReportGenerationExecutionError -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: LastReportGenerationExecutionError)
{-# DEPRECATED lrgeeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON LastReportGenerationExecutionError where
  parseJSON =
    Lude.withObject
      "LastReportGenerationExecutionError"
      ( \x ->
          LastReportGenerationExecutionError'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "ErrorMessage")
      )
