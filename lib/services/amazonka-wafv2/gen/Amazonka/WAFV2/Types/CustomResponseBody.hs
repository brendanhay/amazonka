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
-- Module      : Amazonka.WAFV2.Types.CustomResponseBody
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.CustomResponseBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.ResponseContentType

-- | The response body to use in a custom response to a web request. This is
-- referenced by key from CustomResponse @CustomResponseBodyKey@.
--
-- /See:/ 'newCustomResponseBody' smart constructor.
data CustomResponseBody = CustomResponseBody'
  { -- | The type of content in the payload that you are defining in the
    -- @Content@ string.
    contentType :: ResponseContentType,
    -- | The payload of the custom response.
    --
    -- You can use JSON escape strings in JSON content. To do this, you must
    -- specify JSON content in the @ContentType@ setting.
    --
    -- For information about the limits on count and size for custom request
    -- and response settings, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomResponseBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'customResponseBody_contentType' - The type of content in the payload that you are defining in the
-- @Content@ string.
--
-- 'content', 'customResponseBody_content' - The payload of the custom response.
--
-- You can use JSON escape strings in JSON content. To do this, you must
-- specify JSON content in the @ContentType@ setting.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
newCustomResponseBody ::
  -- | 'contentType'
  ResponseContentType ->
  -- | 'content'
  Prelude.Text ->
  CustomResponseBody
newCustomResponseBody pContentType_ pContent_ =
  CustomResponseBody'
    { contentType = pContentType_,
      content = pContent_
    }

-- | The type of content in the payload that you are defining in the
-- @Content@ string.
customResponseBody_contentType :: Lens.Lens' CustomResponseBody ResponseContentType
customResponseBody_contentType = Lens.lens (\CustomResponseBody' {contentType} -> contentType) (\s@CustomResponseBody' {} a -> s {contentType = a} :: CustomResponseBody)

-- | The payload of the custom response.
--
-- You can use JSON escape strings in JSON content. To do this, you must
-- specify JSON content in the @ContentType@ setting.
--
-- For information about the limits on count and size for custom request
-- and response settings, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/limits.html WAF quotas>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
customResponseBody_content :: Lens.Lens' CustomResponseBody Prelude.Text
customResponseBody_content = Lens.lens (\CustomResponseBody' {content} -> content) (\s@CustomResponseBody' {} a -> s {content = a} :: CustomResponseBody)

instance Data.FromJSON CustomResponseBody where
  parseJSON =
    Data.withObject
      "CustomResponseBody"
      ( \x ->
          CustomResponseBody'
            Prelude.<$> (x Data..: "ContentType")
            Prelude.<*> (x Data..: "Content")
      )

instance Prelude.Hashable CustomResponseBody where
  hashWithSalt _salt CustomResponseBody' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` content

instance Prelude.NFData CustomResponseBody where
  rnf CustomResponseBody' {..} =
    Prelude.rnf contentType `Prelude.seq`
      Prelude.rnf content

instance Data.ToJSON CustomResponseBody where
  toJSON CustomResponseBody' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContentType" Data..= contentType),
            Prelude.Just ("Content" Data..= content)
          ]
      )
