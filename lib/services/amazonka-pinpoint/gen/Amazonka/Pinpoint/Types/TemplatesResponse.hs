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
-- Module      : Amazonka.Pinpoint.Types.TemplatesResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplatesResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.TemplateResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about all the message templates that are associated
-- with your Amazon Pinpoint account.
--
-- /See:/ 'newTemplatesResponse' smart constructor.
data TemplatesResponse = TemplatesResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each message template that\'s associated
    -- with your Amazon Pinpoint account and meets any filter criteria that you
    -- specified in the request.
    item :: [TemplateResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'templatesResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'item', 'templatesResponse_item' - An array of responses, one for each message template that\'s associated
-- with your Amazon Pinpoint account and meets any filter criteria that you
-- specified in the request.
newTemplatesResponse ::
  TemplatesResponse
newTemplatesResponse =
  TemplatesResponse'
    { nextToken = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
templatesResponse_nextToken :: Lens.Lens' TemplatesResponse (Prelude.Maybe Prelude.Text)
templatesResponse_nextToken = Lens.lens (\TemplatesResponse' {nextToken} -> nextToken) (\s@TemplatesResponse' {} a -> s {nextToken = a} :: TemplatesResponse)

-- | An array of responses, one for each message template that\'s associated
-- with your Amazon Pinpoint account and meets any filter criteria that you
-- specified in the request.
templatesResponse_item :: Lens.Lens' TemplatesResponse [TemplateResponse]
templatesResponse_item = Lens.lens (\TemplatesResponse' {item} -> item) (\s@TemplatesResponse' {} a -> s {item = a} :: TemplatesResponse) Prelude.. Lens.coerced

instance Data.FromJSON TemplatesResponse where
  parseJSON =
    Data.withObject
      "TemplatesResponse"
      ( \x ->
          TemplatesResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TemplatesResponse where
  hashWithSalt _salt TemplatesResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` item

instance Prelude.NFData TemplatesResponse where
  rnf TemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf item
