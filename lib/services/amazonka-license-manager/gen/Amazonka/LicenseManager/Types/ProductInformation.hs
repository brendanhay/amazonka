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
-- Module      : Amazonka.LicenseManager.Types.ProductInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ProductInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManager.Types.ProductInformationFilter
import qualified Amazonka.Prelude as Prelude

-- | Describes product information for a license configuration.
--
-- /See:/ 'newProductInformation' smart constructor.
data ProductInformation = ProductInformation'
  { -- | Resource type. The possible values are @SSM_MANAGED@ | @RDS@.
    resourceType :: Prelude.Text,
    -- | A Product information filter consists of a
    -- @ProductInformationFilterComparator@ which is a logical operator, a
    -- @ProductInformationFilterName@ which specifies the type of filter being
    -- declared, and a @ProductInformationFilterValue@ that specifies the value
    -- to filter on.
    --
    -- Accepted values for @ProductInformationFilterName@ are listed here along
    -- with descriptions and valid options for
    -- @ProductInformationFilterComparator@.
    --
    -- The following filters and are supported when the resource type is
    -- @SSM_MANAGED@:
    --
    -- -   @Application Name@ - The name of the application. Logical operator
    --     is @EQUALS@.
    --
    -- -   @Application Publisher@ - The publisher of the application. Logical
    --     operator is @EQUALS@.
    --
    -- -   @Application Version@ - The version of the application. Logical
    --     operator is @EQUALS@.
    --
    -- -   @Platform Name@ - The name of the platform. Logical operator is
    --     @EQUALS@.
    --
    -- -   @Platform Type@ - The platform type. Logical operator is @EQUALS@.
    --
    -- -   @Tag:key@ - The key of a tag attached to an Amazon Web Services
    --     resource you wish to exclude from automated discovery. Logical
    --     operator is @NOT_EQUALS@. The key for your tag must be appended to
    --     @Tag:@ following the example: @Tag:name-of-your-key@.
    --     @ProductInformationFilterValue@ is optional if you are not using
    --     values for the key.
    --
    -- -   @AccountId@ - The 12-digit ID of an Amazon Web Services account you
    --     wish to exclude from automated discovery. Logical operator is
    --     @NOT_EQUALS@.
    --
    -- -   @License Included@ - The type of license included. Logical operators
    --     are @EQUALS@ and @NOT_EQUALS@. Possible values are:
    --     @sql-server-enterprise@ | @sql-server-standard@ | @sql-server-web@ |
    --     @windows-server-datacenter@.
    --
    -- The following filters and logical operators are supported when the
    -- resource type is @RDS@:
    --
    -- -   @Engine Edition@ - The edition of the database engine. Logical
    --     operator is @EQUALS@. Possible values are: @oracle-ee@ | @oracle-se@
    --     | @oracle-se1@ | @oracle-se2@.
    --
    -- -   @License Pack@ - The license pack. Logical operator is @EQUALS@.
    --     Possible values are: @data guard@ | @diagnostic pack sqlt@ |
    --     @tuning pack sqlt@ | @ols@ | @olap@.
    productInformationFilterList :: [ProductInformationFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'productInformation_resourceType' - Resource type. The possible values are @SSM_MANAGED@ | @RDS@.
--
-- 'productInformationFilterList', 'productInformation_productInformationFilterList' - A Product information filter consists of a
-- @ProductInformationFilterComparator@ which is a logical operator, a
-- @ProductInformationFilterName@ which specifies the type of filter being
-- declared, and a @ProductInformationFilterValue@ that specifies the value
-- to filter on.
--
-- Accepted values for @ProductInformationFilterName@ are listed here along
-- with descriptions and valid options for
-- @ProductInformationFilterComparator@.
--
-- The following filters and are supported when the resource type is
-- @SSM_MANAGED@:
--
-- -   @Application Name@ - The name of the application. Logical operator
--     is @EQUALS@.
--
-- -   @Application Publisher@ - The publisher of the application. Logical
--     operator is @EQUALS@.
--
-- -   @Application Version@ - The version of the application. Logical
--     operator is @EQUALS@.
--
-- -   @Platform Name@ - The name of the platform. Logical operator is
--     @EQUALS@.
--
-- -   @Platform Type@ - The platform type. Logical operator is @EQUALS@.
--
-- -   @Tag:key@ - The key of a tag attached to an Amazon Web Services
--     resource you wish to exclude from automated discovery. Logical
--     operator is @NOT_EQUALS@. The key for your tag must be appended to
--     @Tag:@ following the example: @Tag:name-of-your-key@.
--     @ProductInformationFilterValue@ is optional if you are not using
--     values for the key.
--
-- -   @AccountId@ - The 12-digit ID of an Amazon Web Services account you
--     wish to exclude from automated discovery. Logical operator is
--     @NOT_EQUALS@.
--
-- -   @License Included@ - The type of license included. Logical operators
--     are @EQUALS@ and @NOT_EQUALS@. Possible values are:
--     @sql-server-enterprise@ | @sql-server-standard@ | @sql-server-web@ |
--     @windows-server-datacenter@.
--
-- The following filters and logical operators are supported when the
-- resource type is @RDS@:
--
-- -   @Engine Edition@ - The edition of the database engine. Logical
--     operator is @EQUALS@. Possible values are: @oracle-ee@ | @oracle-se@
--     | @oracle-se1@ | @oracle-se2@.
--
-- -   @License Pack@ - The license pack. Logical operator is @EQUALS@.
--     Possible values are: @data guard@ | @diagnostic pack sqlt@ |
--     @tuning pack sqlt@ | @ols@ | @olap@.
newProductInformation ::
  -- | 'resourceType'
  Prelude.Text ->
  ProductInformation
newProductInformation pResourceType_ =
  ProductInformation'
    { resourceType = pResourceType_,
      productInformationFilterList = Prelude.mempty
    }

-- | Resource type. The possible values are @SSM_MANAGED@ | @RDS@.
productInformation_resourceType :: Lens.Lens' ProductInformation Prelude.Text
productInformation_resourceType = Lens.lens (\ProductInformation' {resourceType} -> resourceType) (\s@ProductInformation' {} a -> s {resourceType = a} :: ProductInformation)

-- | A Product information filter consists of a
-- @ProductInformationFilterComparator@ which is a logical operator, a
-- @ProductInformationFilterName@ which specifies the type of filter being
-- declared, and a @ProductInformationFilterValue@ that specifies the value
-- to filter on.
--
-- Accepted values for @ProductInformationFilterName@ are listed here along
-- with descriptions and valid options for
-- @ProductInformationFilterComparator@.
--
-- The following filters and are supported when the resource type is
-- @SSM_MANAGED@:
--
-- -   @Application Name@ - The name of the application. Logical operator
--     is @EQUALS@.
--
-- -   @Application Publisher@ - The publisher of the application. Logical
--     operator is @EQUALS@.
--
-- -   @Application Version@ - The version of the application. Logical
--     operator is @EQUALS@.
--
-- -   @Platform Name@ - The name of the platform. Logical operator is
--     @EQUALS@.
--
-- -   @Platform Type@ - The platform type. Logical operator is @EQUALS@.
--
-- -   @Tag:key@ - The key of a tag attached to an Amazon Web Services
--     resource you wish to exclude from automated discovery. Logical
--     operator is @NOT_EQUALS@. The key for your tag must be appended to
--     @Tag:@ following the example: @Tag:name-of-your-key@.
--     @ProductInformationFilterValue@ is optional if you are not using
--     values for the key.
--
-- -   @AccountId@ - The 12-digit ID of an Amazon Web Services account you
--     wish to exclude from automated discovery. Logical operator is
--     @NOT_EQUALS@.
--
-- -   @License Included@ - The type of license included. Logical operators
--     are @EQUALS@ and @NOT_EQUALS@. Possible values are:
--     @sql-server-enterprise@ | @sql-server-standard@ | @sql-server-web@ |
--     @windows-server-datacenter@.
--
-- The following filters and logical operators are supported when the
-- resource type is @RDS@:
--
-- -   @Engine Edition@ - The edition of the database engine. Logical
--     operator is @EQUALS@. Possible values are: @oracle-ee@ | @oracle-se@
--     | @oracle-se1@ | @oracle-se2@.
--
-- -   @License Pack@ - The license pack. Logical operator is @EQUALS@.
--     Possible values are: @data guard@ | @diagnostic pack sqlt@ |
--     @tuning pack sqlt@ | @ols@ | @olap@.
productInformation_productInformationFilterList :: Lens.Lens' ProductInformation [ProductInformationFilter]
productInformation_productInformationFilterList = Lens.lens (\ProductInformation' {productInformationFilterList} -> productInformationFilterList) (\s@ProductInformation' {} a -> s {productInformationFilterList = a} :: ProductInformation) Prelude.. Lens.coerced

instance Core.FromJSON ProductInformation where
  parseJSON =
    Core.withObject
      "ProductInformation"
      ( \x ->
          ProductInformation'
            Prelude.<$> (x Core..: "ResourceType")
            Prelude.<*> ( x Core..:? "ProductInformationFilterList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProductInformation where
  hashWithSalt _salt ProductInformation' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` productInformationFilterList

instance Prelude.NFData ProductInformation where
  rnf ProductInformation' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf productInformationFilterList

instance Core.ToJSON ProductInformation where
  toJSON ProductInformation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceType" Core..= resourceType),
            Prelude.Just
              ( "ProductInformationFilterList"
                  Core..= productInformationFilterList
              )
          ]
      )
