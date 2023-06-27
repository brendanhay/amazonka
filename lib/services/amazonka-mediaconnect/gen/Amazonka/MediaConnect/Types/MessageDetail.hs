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
-- Module      : Amazonka.MediaConnect.Types.MessageDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MessageDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newMessageDetail' smart constructor.
data MessageDetail = MessageDetail'
  { -- | The name of the resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The specific error message that MediaConnect returns to help you
    -- understand the reason that the request did not succeed.
    message :: Prelude.Text,
    -- | The error code.
    code :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'messageDetail_resourceName' - The name of the resource.
--
-- 'message', 'messageDetail_message' - The specific error message that MediaConnect returns to help you
-- understand the reason that the request did not succeed.
--
-- 'code', 'messageDetail_code' - The error code.
newMessageDetail ::
  -- | 'message'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  MessageDetail
newMessageDetail pMessage_ pCode_ =
  MessageDetail'
    { resourceName = Prelude.Nothing,
      message = pMessage_,
      code = pCode_
    }

-- | The name of the resource.
messageDetail_resourceName :: Lens.Lens' MessageDetail (Prelude.Maybe Prelude.Text)
messageDetail_resourceName = Lens.lens (\MessageDetail' {resourceName} -> resourceName) (\s@MessageDetail' {} a -> s {resourceName = a} :: MessageDetail)

-- | The specific error message that MediaConnect returns to help you
-- understand the reason that the request did not succeed.
messageDetail_message :: Lens.Lens' MessageDetail Prelude.Text
messageDetail_message = Lens.lens (\MessageDetail' {message} -> message) (\s@MessageDetail' {} a -> s {message = a} :: MessageDetail)

-- | The error code.
messageDetail_code :: Lens.Lens' MessageDetail Prelude.Text
messageDetail_code = Lens.lens (\MessageDetail' {code} -> code) (\s@MessageDetail' {} a -> s {code = a} :: MessageDetail)

instance Data.FromJSON MessageDetail where
  parseJSON =
    Data.withObject
      "MessageDetail"
      ( \x ->
          MessageDetail'
            Prelude.<$> (x Data..:? "resourceName")
            Prelude.<*> (x Data..: "message")
            Prelude.<*> (x Data..: "code")
      )

instance Prelude.Hashable MessageDetail where
  hashWithSalt _salt MessageDetail' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData MessageDetail where
  rnf MessageDetail' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf code
