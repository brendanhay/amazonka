{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RenderingError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RenderingError where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A description of an error that occurred while rendering the template.
--
-- /See:/ 'newRenderingError' smart constructor.
data RenderingError = RenderingError'
  { -- | A unique identifier for a specific class of errors.
    code :: Prelude.Text,
    -- | A human-readable message describing the error.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RenderingError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'renderingError_code' - A unique identifier for a specific class of errors.
--
-- 'message', 'renderingError_message' - A human-readable message describing the error.
newRenderingError ::
  -- | 'code'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  RenderingError
newRenderingError pCode_ pMessage_ =
  RenderingError' {code = pCode_, message = pMessage_}

-- | A unique identifier for a specific class of errors.
renderingError_code :: Lens.Lens' RenderingError Prelude.Text
renderingError_code = Lens.lens (\RenderingError' {code} -> code) (\s@RenderingError' {} a -> s {code = a} :: RenderingError)

-- | A human-readable message describing the error.
renderingError_message :: Lens.Lens' RenderingError Prelude.Text
renderingError_message = Lens.lens (\RenderingError' {message} -> message) (\s@RenderingError' {} a -> s {message = a} :: RenderingError)

instance Prelude.FromJSON RenderingError where
  parseJSON =
    Prelude.withObject
      "RenderingError"
      ( \x ->
          RenderingError'
            Prelude.<$> (x Prelude..: "Code")
            Prelude.<*> (x Prelude..: "Message")
      )

instance Prelude.Hashable RenderingError

instance Prelude.NFData RenderingError
