{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.GetCustomEntityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a custom pattern by specifying its name.
module Amazonka.Glue.GetCustomEntityType
  ( -- * Creating a Request
    GetCustomEntityType (..),
    newGetCustomEntityType,

    -- * Request Lenses
    getCustomEntityType_name,

    -- * Destructuring the Response
    GetCustomEntityTypeResponse (..),
    newGetCustomEntityTypeResponse,

    -- * Response Lenses
    getCustomEntityTypeResponse_contextWords,
    getCustomEntityTypeResponse_name,
    getCustomEntityTypeResponse_regexString,
    getCustomEntityTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCustomEntityType' smart constructor.
data GetCustomEntityType = GetCustomEntityType'
  { -- | The name of the custom pattern that you want to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCustomEntityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getCustomEntityType_name' - The name of the custom pattern that you want to retrieve.
newGetCustomEntityType ::
  -- | 'name'
  Prelude.Text ->
  GetCustomEntityType
newGetCustomEntityType pName_ =
  GetCustomEntityType' {name = pName_}

-- | The name of the custom pattern that you want to retrieve.
getCustomEntityType_name :: Lens.Lens' GetCustomEntityType Prelude.Text
getCustomEntityType_name = Lens.lens (\GetCustomEntityType' {name} -> name) (\s@GetCustomEntityType' {} a -> s {name = a} :: GetCustomEntityType)

instance Core.AWSRequest GetCustomEntityType where
  type
    AWSResponse GetCustomEntityType =
      GetCustomEntityTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCustomEntityTypeResponse'
            Prelude.<$> (x Data..?> "ContextWords")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RegexString")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCustomEntityType where
  hashWithSalt _salt GetCustomEntityType' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetCustomEntityType where
  rnf GetCustomEntityType' {..} = Prelude.rnf name

instance Data.ToHeaders GetCustomEntityType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetCustomEntityType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCustomEntityType where
  toJSON GetCustomEntityType' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetCustomEntityType where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCustomEntityType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCustomEntityTypeResponse' smart constructor.
data GetCustomEntityTypeResponse = GetCustomEntityTypeResponse'
  { -- | A list of context words if specified when you created the custom
    -- pattern. If none of these context words are found within the vicinity of
    -- the regular expression the data will not be detected as sensitive data.
    contextWords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the custom pattern that you retrieved.
    name :: Prelude.Maybe Prelude.Text,
    -- | A regular expression string that is used for detecting sensitive data in
    -- a custom pattern.
    regexString :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCustomEntityTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextWords', 'getCustomEntityTypeResponse_contextWords' - A list of context words if specified when you created the custom
-- pattern. If none of these context words are found within the vicinity of
-- the regular expression the data will not be detected as sensitive data.
--
-- 'name', 'getCustomEntityTypeResponse_name' - The name of the custom pattern that you retrieved.
--
-- 'regexString', 'getCustomEntityTypeResponse_regexString' - A regular expression string that is used for detecting sensitive data in
-- a custom pattern.
--
-- 'httpStatus', 'getCustomEntityTypeResponse_httpStatus' - The response's http status code.
newGetCustomEntityTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCustomEntityTypeResponse
newGetCustomEntityTypeResponse pHttpStatus_ =
  GetCustomEntityTypeResponse'
    { contextWords =
        Prelude.Nothing,
      name = Prelude.Nothing,
      regexString = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of context words if specified when you created the custom
-- pattern. If none of these context words are found within the vicinity of
-- the regular expression the data will not be detected as sensitive data.
getCustomEntityTypeResponse_contextWords :: Lens.Lens' GetCustomEntityTypeResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getCustomEntityTypeResponse_contextWords = Lens.lens (\GetCustomEntityTypeResponse' {contextWords} -> contextWords) (\s@GetCustomEntityTypeResponse' {} a -> s {contextWords = a} :: GetCustomEntityTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the custom pattern that you retrieved.
getCustomEntityTypeResponse_name :: Lens.Lens' GetCustomEntityTypeResponse (Prelude.Maybe Prelude.Text)
getCustomEntityTypeResponse_name = Lens.lens (\GetCustomEntityTypeResponse' {name} -> name) (\s@GetCustomEntityTypeResponse' {} a -> s {name = a} :: GetCustomEntityTypeResponse)

-- | A regular expression string that is used for detecting sensitive data in
-- a custom pattern.
getCustomEntityTypeResponse_regexString :: Lens.Lens' GetCustomEntityTypeResponse (Prelude.Maybe Prelude.Text)
getCustomEntityTypeResponse_regexString = Lens.lens (\GetCustomEntityTypeResponse' {regexString} -> regexString) (\s@GetCustomEntityTypeResponse' {} a -> s {regexString = a} :: GetCustomEntityTypeResponse)

-- | The response's http status code.
getCustomEntityTypeResponse_httpStatus :: Lens.Lens' GetCustomEntityTypeResponse Prelude.Int
getCustomEntityTypeResponse_httpStatus = Lens.lens (\GetCustomEntityTypeResponse' {httpStatus} -> httpStatus) (\s@GetCustomEntityTypeResponse' {} a -> s {httpStatus = a} :: GetCustomEntityTypeResponse)

instance Prelude.NFData GetCustomEntityTypeResponse where
  rnf GetCustomEntityTypeResponse' {..} =
    Prelude.rnf contextWords
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regexString
      `Prelude.seq` Prelude.rnf httpStatus
