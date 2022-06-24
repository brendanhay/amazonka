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
-- Module      : Amazonka.Pinpoint.Types.TemplateVersionsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateVersionsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.TemplateVersionResponse
import qualified Amazonka.Prelude as Prelude

-- | Provides information about all the versions of a specific message
-- template.
--
-- /See:/ 'newTemplateVersionsResponse' smart constructor.
data TemplateVersionsResponse = TemplateVersionsResponse'
  { -- | The message that\'s returned from the API for the request to retrieve
    -- information about all the versions of the message template.
    message :: Prelude.Maybe Prelude.Text,
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the request to retrieve information about all
    -- the versions of the message template.
    requestID :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each version of the message template.
    item :: [TemplateVersionResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'templateVersionsResponse_message' - The message that\'s returned from the API for the request to retrieve
-- information about all the versions of the message template.
--
-- 'nextToken', 'templateVersionsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'requestID', 'templateVersionsResponse_requestID' - The unique identifier for the request to retrieve information about all
-- the versions of the message template.
--
-- 'item', 'templateVersionsResponse_item' - An array of responses, one for each version of the message template.
newTemplateVersionsResponse ::
  TemplateVersionsResponse
newTemplateVersionsResponse =
  TemplateVersionsResponse'
    { message =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestID = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The message that\'s returned from the API for the request to retrieve
-- information about all the versions of the message template.
templateVersionsResponse_message :: Lens.Lens' TemplateVersionsResponse (Prelude.Maybe Prelude.Text)
templateVersionsResponse_message = Lens.lens (\TemplateVersionsResponse' {message} -> message) (\s@TemplateVersionsResponse' {} a -> s {message = a} :: TemplateVersionsResponse)

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
templateVersionsResponse_nextToken :: Lens.Lens' TemplateVersionsResponse (Prelude.Maybe Prelude.Text)
templateVersionsResponse_nextToken = Lens.lens (\TemplateVersionsResponse' {nextToken} -> nextToken) (\s@TemplateVersionsResponse' {} a -> s {nextToken = a} :: TemplateVersionsResponse)

-- | The unique identifier for the request to retrieve information about all
-- the versions of the message template.
templateVersionsResponse_requestID :: Lens.Lens' TemplateVersionsResponse (Prelude.Maybe Prelude.Text)
templateVersionsResponse_requestID = Lens.lens (\TemplateVersionsResponse' {requestID} -> requestID) (\s@TemplateVersionsResponse' {} a -> s {requestID = a} :: TemplateVersionsResponse)

-- | An array of responses, one for each version of the message template.
templateVersionsResponse_item :: Lens.Lens' TemplateVersionsResponse [TemplateVersionResponse]
templateVersionsResponse_item = Lens.lens (\TemplateVersionsResponse' {item} -> item) (\s@TemplateVersionsResponse' {} a -> s {item = a} :: TemplateVersionsResponse) Prelude.. Lens.coerced

instance Core.FromJSON TemplateVersionsResponse where
  parseJSON =
    Core.withObject
      "TemplateVersionsResponse"
      ( \x ->
          TemplateVersionsResponse'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "NextToken")
            Prelude.<*> (x Core..:? "RequestID")
            Prelude.<*> (x Core..:? "Item" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TemplateVersionsResponse where
  hashWithSalt _salt TemplateVersionsResponse' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` requestID
      `Prelude.hashWithSalt` item

instance Prelude.NFData TemplateVersionsResponse where
  rnf TemplateVersionsResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestID
      `Prelude.seq` Prelude.rnf item
