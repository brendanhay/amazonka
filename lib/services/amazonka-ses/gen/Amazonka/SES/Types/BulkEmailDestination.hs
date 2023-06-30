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
-- Module      : Amazonka.SES.Types.BulkEmailDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.BulkEmailDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.Destination
import Amazonka.SES.Types.MessageTag

-- | An array that contains one or more Destinations, as well as the tags and
-- replacement data associated with each of those Destinations.
--
-- /See:/ 'newBulkEmailDestination' smart constructor.
data BulkEmailDestination = BulkEmailDestination'
  { -- | A list of tags, in the form of name\/value pairs, to apply to an email
    -- that you send using @SendBulkTemplatedEmail@. Tags correspond to
    -- characteristics of the email that you define, so that you can publish
    -- email sending events.
    replacementTags :: Prelude.Maybe [MessageTag],
    -- | A list of replacement values to apply to the template. This parameter is
    -- a JSON object, typically consisting of key-value pairs in which the keys
    -- correspond to replacement tags in the email template.
    replacementTemplateData :: Prelude.Maybe Prelude.Text,
    destination :: Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BulkEmailDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replacementTags', 'bulkEmailDestination_replacementTags' - A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendBulkTemplatedEmail@. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
--
-- 'replacementTemplateData', 'bulkEmailDestination_replacementTemplateData' - A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
--
-- 'destination', 'bulkEmailDestination_destination' - Undocumented member.
newBulkEmailDestination ::
  -- | 'destination'
  Destination ->
  BulkEmailDestination
newBulkEmailDestination pDestination_ =
  BulkEmailDestination'
    { replacementTags =
        Prelude.Nothing,
      replacementTemplateData = Prelude.Nothing,
      destination = pDestination_
    }

-- | A list of tags, in the form of name\/value pairs, to apply to an email
-- that you send using @SendBulkTemplatedEmail@. Tags correspond to
-- characteristics of the email that you define, so that you can publish
-- email sending events.
bulkEmailDestination_replacementTags :: Lens.Lens' BulkEmailDestination (Prelude.Maybe [MessageTag])
bulkEmailDestination_replacementTags = Lens.lens (\BulkEmailDestination' {replacementTags} -> replacementTags) (\s@BulkEmailDestination' {} a -> s {replacementTags = a} :: BulkEmailDestination) Prelude.. Lens.mapping Lens.coerced

-- | A list of replacement values to apply to the template. This parameter is
-- a JSON object, typically consisting of key-value pairs in which the keys
-- correspond to replacement tags in the email template.
bulkEmailDestination_replacementTemplateData :: Lens.Lens' BulkEmailDestination (Prelude.Maybe Prelude.Text)
bulkEmailDestination_replacementTemplateData = Lens.lens (\BulkEmailDestination' {replacementTemplateData} -> replacementTemplateData) (\s@BulkEmailDestination' {} a -> s {replacementTemplateData = a} :: BulkEmailDestination)

-- | Undocumented member.
bulkEmailDestination_destination :: Lens.Lens' BulkEmailDestination Destination
bulkEmailDestination_destination = Lens.lens (\BulkEmailDestination' {destination} -> destination) (\s@BulkEmailDestination' {} a -> s {destination = a} :: BulkEmailDestination)

instance Prelude.Hashable BulkEmailDestination where
  hashWithSalt _salt BulkEmailDestination' {..} =
    _salt
      `Prelude.hashWithSalt` replacementTags
      `Prelude.hashWithSalt` replacementTemplateData
      `Prelude.hashWithSalt` destination

instance Prelude.NFData BulkEmailDestination where
  rnf BulkEmailDestination' {..} =
    Prelude.rnf replacementTags
      `Prelude.seq` Prelude.rnf replacementTemplateData
      `Prelude.seq` Prelude.rnf destination

instance Data.ToQuery BulkEmailDestination where
  toQuery BulkEmailDestination' {..} =
    Prelude.mconcat
      [ "ReplacementTags"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> replacementTags
            ),
        "ReplacementTemplateData"
          Data.=: replacementTemplateData,
        "Destination" Data.=: destination
      ]
