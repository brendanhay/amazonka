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
-- Module      : Network.AWS.Pinpoint.Types.TemplateVersionsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateVersionsResponse where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateVersionResponse
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about all the versions of a specific message
-- template.
--
-- /See:/ 'newTemplateVersionsResponse' smart constructor.
data TemplateVersionsResponse = TemplateVersionsResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The message that\'s returned from the API for the request to retrieve
    -- information about all the versions of the message template.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the request to retrieve information about all
    -- the versions of the message template.
    requestID :: Prelude.Maybe Prelude.Text,
    -- | An array of responses, one for each version of the message template.
    item :: [TemplateVersionResponse]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'templateVersionsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'message', 'templateVersionsResponse_message' - The message that\'s returned from the API for the request to retrieve
-- information about all the versions of the message template.
--
-- 'requestID', 'templateVersionsResponse_requestID' - The unique identifier for the request to retrieve information about all
-- the versions of the message template.
--
-- 'item', 'templateVersionsResponse_item' - An array of responses, one for each version of the message template.
newTemplateVersionsResponse ::
  TemplateVersionsResponse
newTemplateVersionsResponse =
  TemplateVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      message = Prelude.Nothing,
      requestID = Prelude.Nothing,
      item = Prelude.mempty
    }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
templateVersionsResponse_nextToken :: Lens.Lens' TemplateVersionsResponse (Prelude.Maybe Prelude.Text)
templateVersionsResponse_nextToken = Lens.lens (\TemplateVersionsResponse' {nextToken} -> nextToken) (\s@TemplateVersionsResponse' {} a -> s {nextToken = a} :: TemplateVersionsResponse)

-- | The message that\'s returned from the API for the request to retrieve
-- information about all the versions of the message template.
templateVersionsResponse_message :: Lens.Lens' TemplateVersionsResponse (Prelude.Maybe Prelude.Text)
templateVersionsResponse_message = Lens.lens (\TemplateVersionsResponse' {message} -> message) (\s@TemplateVersionsResponse' {} a -> s {message = a} :: TemplateVersionsResponse)

-- | The unique identifier for the request to retrieve information about all
-- the versions of the message template.
templateVersionsResponse_requestID :: Lens.Lens' TemplateVersionsResponse (Prelude.Maybe Prelude.Text)
templateVersionsResponse_requestID = Lens.lens (\TemplateVersionsResponse' {requestID} -> requestID) (\s@TemplateVersionsResponse' {} a -> s {requestID = a} :: TemplateVersionsResponse)

-- | An array of responses, one for each version of the message template.
templateVersionsResponse_item :: Lens.Lens' TemplateVersionsResponse [TemplateVersionResponse]
templateVersionsResponse_item = Lens.lens (\TemplateVersionsResponse' {item} -> item) (\s@TemplateVersionsResponse' {} a -> s {item = a} :: TemplateVersionsResponse) Prelude.. Prelude._Coerce

instance Prelude.FromJSON TemplateVersionsResponse where
  parseJSON =
    Prelude.withObject
      "TemplateVersionsResponse"
      ( \x ->
          TemplateVersionsResponse'
            Prelude.<$> (x Prelude..:? "NextToken")
            Prelude.<*> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "RequestID")
            Prelude.<*> (x Prelude..:? "Item" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable TemplateVersionsResponse

instance Prelude.NFData TemplateVersionsResponse
