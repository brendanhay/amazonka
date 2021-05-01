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
-- Module      : Network.AWS.Pinpoint.Types.ApplicationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ApplicationResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about an application.
--
-- /See:/ 'newApplicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { -- | A string-to-string map of key-value pairs that identifies the tags that
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
    { tags = Prelude.Nothing,
      id = pId_,
      arn = pArn_,
      name = pName_
    }

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the application. Each tag consists of a required tag
-- key and an associated tag value.
applicationResponse_tags :: Lens.Lens' ApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
applicationResponse_tags = Lens.lens (\ApplicationResponse' {tags} -> tags) (\s@ApplicationResponse' {} a -> s {tags = a} :: ApplicationResponse) Prelude.. Lens.mapping Prelude._Coerce

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

instance Prelude.FromJSON ApplicationResponse where
  parseJSON =
    Prelude.withObject
      "ApplicationResponse"
      ( \x ->
          ApplicationResponse'
            Prelude.<$> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "Arn")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable ApplicationResponse

instance Prelude.NFData ApplicationResponse
