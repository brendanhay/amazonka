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
-- Module      : Network.AWS.Support.Types.SupportService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.SupportService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Support.Types.Category

-- | Information about an AWS service returned by the DescribeServices
-- operation.
--
-- /See:/ 'newSupportService' smart constructor.
data SupportService = SupportService'
  { -- | The code for an AWS service returned by the DescribeServices response.
    -- The @name@ element contains the corresponding friendly name.
    code :: Core.Maybe Core.Text,
    -- | The friendly name for an AWS service. The @code@ element contains the
    -- corresponding code.
    name :: Core.Maybe Core.Text,
    -- | A list of categories that describe the type of support issue a case
    -- describes. Categories consist of a category name and a category code.
    -- Category names and codes are passed to AWS Support when you call
    -- CreateCase.
    categories :: Core.Maybe [Category]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SupportService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'supportService_code' - The code for an AWS service returned by the DescribeServices response.
-- The @name@ element contains the corresponding friendly name.
--
-- 'name', 'supportService_name' - The friendly name for an AWS service. The @code@ element contains the
-- corresponding code.
--
-- 'categories', 'supportService_categories' - A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
newSupportService ::
  SupportService
newSupportService =
  SupportService'
    { code = Core.Nothing,
      name = Core.Nothing,
      categories = Core.Nothing
    }

-- | The code for an AWS service returned by the DescribeServices response.
-- The @name@ element contains the corresponding friendly name.
supportService_code :: Lens.Lens' SupportService (Core.Maybe Core.Text)
supportService_code = Lens.lens (\SupportService' {code} -> code) (\s@SupportService' {} a -> s {code = a} :: SupportService)

-- | The friendly name for an AWS service. The @code@ element contains the
-- corresponding code.
supportService_name :: Lens.Lens' SupportService (Core.Maybe Core.Text)
supportService_name = Lens.lens (\SupportService' {name} -> name) (\s@SupportService' {} a -> s {name = a} :: SupportService)

-- | A list of categories that describe the type of support issue a case
-- describes. Categories consist of a category name and a category code.
-- Category names and codes are passed to AWS Support when you call
-- CreateCase.
supportService_categories :: Lens.Lens' SupportService (Core.Maybe [Category])
supportService_categories = Lens.lens (\SupportService' {categories} -> categories) (\s@SupportService' {} a -> s {categories = a} :: SupportService) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SupportService where
  parseJSON =
    Core.withObject
      "SupportService"
      ( \x ->
          SupportService'
            Core.<$> (x Core..:? "code")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "categories" Core..!= Core.mempty)
      )

instance Core.Hashable SupportService

instance Core.NFData SupportService
