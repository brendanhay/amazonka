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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about an application.
--
-- /See:/ 'newApplicationResponse' smart constructor.
data ApplicationResponse = ApplicationResponse'
  { -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the application. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    id :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    arn :: Core.Text,
    -- | The display name of the application. This name is displayed as the
    -- __Project name__ on the Amazon Pinpoint console.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  ApplicationResponse
newApplicationResponse pId_ pArn_ pName_ =
  ApplicationResponse'
    { tags = Core.Nothing,
      id = pId_,
      arn = pArn_,
      name = pName_
    }

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the application. Each tag consists of a required tag
-- key and an associated tag value.
applicationResponse_tags :: Lens.Lens' ApplicationResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
applicationResponse_tags = Lens.lens (\ApplicationResponse' {tags} -> tags) (\s@ApplicationResponse' {} a -> s {tags = a} :: ApplicationResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
applicationResponse_id :: Lens.Lens' ApplicationResponse Core.Text
applicationResponse_id = Lens.lens (\ApplicationResponse' {id} -> id) (\s@ApplicationResponse' {} a -> s {id = a} :: ApplicationResponse)

-- | The Amazon Resource Name (ARN) of the application.
applicationResponse_arn :: Lens.Lens' ApplicationResponse Core.Text
applicationResponse_arn = Lens.lens (\ApplicationResponse' {arn} -> arn) (\s@ApplicationResponse' {} a -> s {arn = a} :: ApplicationResponse)

-- | The display name of the application. This name is displayed as the
-- __Project name__ on the Amazon Pinpoint console.
applicationResponse_name :: Lens.Lens' ApplicationResponse Core.Text
applicationResponse_name = Lens.lens (\ApplicationResponse' {name} -> name) (\s@ApplicationResponse' {} a -> s {name = a} :: ApplicationResponse)

instance Core.FromJSON ApplicationResponse where
  parseJSON =
    Core.withObject
      "ApplicationResponse"
      ( \x ->
          ApplicationResponse'
            Core.<$> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "Arn")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable ApplicationResponse

instance Core.NFData ApplicationResponse
