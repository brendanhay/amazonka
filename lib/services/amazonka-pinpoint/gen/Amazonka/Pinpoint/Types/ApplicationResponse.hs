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
-- Module      : Amazonka.Pinpoint.Types.ApplicationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ApplicationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an application.
--
-- /See:/ 'newApplicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { -- | The date and time when the Application was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the application. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    arn :: Prelude.Text,
    -- | The display name of the application. This name is displayed as the
    -- __Project name__ on the Amazon Pinpoint console.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'applicationResponse_creationDate' - The date and time when the Application was created.
--
-- 'tags', 'applicationResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the application. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'id', 'applicationResponse_id' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'arn', 'applicationResponse_arn' - The Amazon Resource Name (ARN) of the application.
--
-- 'name', 'applicationResponse_name' - The display name of the application. This name is displayed as the
-- __Project name__ on the Amazon Pinpoint console.
newApplicationResponse ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ApplicationResponse
newApplicationResponse pId_ pArn_ pName_ =
  ApplicationResponse'
    { creationDate =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      id = pId_,
      arn = pArn_,
      name = pName_
    }

-- | The date and time when the Application was created.
applicationResponse_creationDate :: Lens.Lens' ApplicationResponse (Prelude.Maybe Prelude.Text)
applicationResponse_creationDate = Lens.lens (\ApplicationResponse' {creationDate} -> creationDate) (\s@ApplicationResponse' {} a -> s {creationDate = a} :: ApplicationResponse)

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the application. Each tag consists of a required tag
-- key and an associated tag value.
applicationResponse_tags :: Lens.Lens' ApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
applicationResponse_tags = Lens.lens (\ApplicationResponse' {tags} -> tags) (\s@ApplicationResponse' {} a -> s {tags = a} :: ApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
applicationResponse_id :: Lens.Lens' ApplicationResponse Prelude.Text
applicationResponse_id = Lens.lens (\ApplicationResponse' {id} -> id) (\s@ApplicationResponse' {} a -> s {id = a} :: ApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application.
applicationResponse_arn :: Lens.Lens' ApplicationResponse Prelude.Text
applicationResponse_arn = Lens.lens (\ApplicationResponse' {arn} -> arn) (\s@ApplicationResponse' {} a -> s {arn = a} :: ApplicationResponse)

-- | The display name of the application. This name is displayed as the
-- __Project name__ on the Amazon Pinpoint console.
applicationResponse_name :: Lens.Lens' ApplicationResponse Prelude.Text
applicationResponse_name = Lens.lens (\ApplicationResponse' {name} -> name) (\s@ApplicationResponse' {} a -> s {name = a} :: ApplicationResponse)

instance Data.FromJSON ApplicationResponse where
  parseJSON =
    Data.withObject
      "ApplicationResponse"
      ( \x ->
          ApplicationResponse'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable ApplicationResponse where
  hashWithSalt _salt ApplicationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name

instance Prelude.NFData ApplicationResponse where
  rnf ApplicationResponse' {..} =
    Prelude.rnf creationDate `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf arn `Prelude.seq`
            Prelude.rnf name
