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
-- Module      : Amazonka.SESV2.Types.ListManagementOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ListManagementOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object used to specify a list or topic to which an email belongs,
-- which will be used when a contact chooses to unsubscribe.
--
-- /See:/ 'newListManagementOptions' smart constructor.
data ListManagementOptions = ListManagementOptions'
  { -- | The name of the topic.
    topicName :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact list.
    contactListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagementOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicName', 'listManagementOptions_topicName' - The name of the topic.
--
-- 'contactListName', 'listManagementOptions_contactListName' - The name of the contact list.
newListManagementOptions ::
  -- | 'contactListName'
  Prelude.Text ->
  ListManagementOptions
newListManagementOptions pContactListName_ =
  ListManagementOptions'
    { topicName = Prelude.Nothing,
      contactListName = pContactListName_
    }

-- | The name of the topic.
listManagementOptions_topicName :: Lens.Lens' ListManagementOptions (Prelude.Maybe Prelude.Text)
listManagementOptions_topicName = Lens.lens (\ListManagementOptions' {topicName} -> topicName) (\s@ListManagementOptions' {} a -> s {topicName = a} :: ListManagementOptions)

-- | The name of the contact list.
listManagementOptions_contactListName :: Lens.Lens' ListManagementOptions Prelude.Text
listManagementOptions_contactListName = Lens.lens (\ListManagementOptions' {contactListName} -> contactListName) (\s@ListManagementOptions' {} a -> s {contactListName = a} :: ListManagementOptions)

instance Prelude.Hashable ListManagementOptions where
  hashWithSalt _salt ListManagementOptions' {..} =
    _salt `Prelude.hashWithSalt` topicName
      `Prelude.hashWithSalt` contactListName

instance Prelude.NFData ListManagementOptions where
  rnf ListManagementOptions' {..} =
    Prelude.rnf topicName
      `Prelude.seq` Prelude.rnf contactListName

instance Data.ToJSON ListManagementOptions where
  toJSON ListManagementOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TopicName" Data..=) Prelude.<$> topicName,
            Prelude.Just
              ("ContactListName" Data..= contactListName)
          ]
      )
