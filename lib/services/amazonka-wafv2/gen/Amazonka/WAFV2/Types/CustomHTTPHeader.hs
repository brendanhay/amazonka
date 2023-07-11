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
-- Module      : Amazonka.WAFV2.Types.CustomHTTPHeader
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CustomHTTPHeader where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom header for custom request and response handling. This is used
-- in CustomResponse and CustomRequestHandling.
--
-- /See:/ 'newCustomHTTPHeader' smart constructor.
data CustomHTTPHeader = CustomHTTPHeader'
  { -- | The name of the custom header.
    --
    -- For custom request header insertion, when WAF inserts the header into
    -- the request, it prefixes this name @x-amzn-waf-@, to avoid confusion
    -- with the headers that are already in the request. For example, for the
    -- header name @sample@, WAF inserts the header @x-amzn-waf-sample@.
    name :: Prelude.Text,
    -- | The value of the custom header.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomHTTPHeader' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'customHTTPHeader_name' - The name of the custom header.
--
-- For custom request header insertion, when WAF inserts the header into
-- the request, it prefixes this name @x-amzn-waf-@, to avoid confusion
-- with the headers that are already in the request. For example, for the
-- header name @sample@, WAF inserts the header @x-amzn-waf-sample@.
--
-- 'value', 'customHTTPHeader_value' - The value of the custom header.
newCustomHTTPHeader ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  CustomHTTPHeader
newCustomHTTPHeader pName_ pValue_ =
  CustomHTTPHeader' {name = pName_, value = pValue_}

-- | The name of the custom header.
--
-- For custom request header insertion, when WAF inserts the header into
-- the request, it prefixes this name @x-amzn-waf-@, to avoid confusion
-- with the headers that are already in the request. For example, for the
-- header name @sample@, WAF inserts the header @x-amzn-waf-sample@.
customHTTPHeader_name :: Lens.Lens' CustomHTTPHeader Prelude.Text
customHTTPHeader_name = Lens.lens (\CustomHTTPHeader' {name} -> name) (\s@CustomHTTPHeader' {} a -> s {name = a} :: CustomHTTPHeader)

-- | The value of the custom header.
customHTTPHeader_value :: Lens.Lens' CustomHTTPHeader Prelude.Text
customHTTPHeader_value = Lens.lens (\CustomHTTPHeader' {value} -> value) (\s@CustomHTTPHeader' {} a -> s {value = a} :: CustomHTTPHeader)

instance Data.FromJSON CustomHTTPHeader where
  parseJSON =
    Data.withObject
      "CustomHTTPHeader"
      ( \x ->
          CustomHTTPHeader'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable CustomHTTPHeader where
  hashWithSalt _salt CustomHTTPHeader' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData CustomHTTPHeader where
  rnf CustomHTTPHeader' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CustomHTTPHeader where
  toJSON CustomHTTPHeader' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
