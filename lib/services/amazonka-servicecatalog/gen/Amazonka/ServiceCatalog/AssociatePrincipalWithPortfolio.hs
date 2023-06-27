{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalog.AssociatePrincipalWithPortfolio
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified principal ARN with the specified portfolio.
--
-- If you share the portfolio with principal name sharing enabled, the
-- @PrincipalARN@ association is included in the share.
--
-- The @PortfolioID@, @PrincipalARN@, and @PrincipalType@ parameters are
-- required.
--
-- You can associate a maximum of 10 Principals with a portfolio using
-- @PrincipalType@ as @IAM_PATTERN@
--
-- When you associate a principal with portfolio, a potential privilege
-- escalation path may occur when that portfolio is then shared with other
-- accounts. For a user in a recipient account who is /not/ an Service
-- Catalog Admin, but still has the ability to create Principals
-- (Users\/Groups\/Roles), that user could create a role that matches a
-- principal name association for the portfolio. Although this user may not
-- know which principal names are associated through Service Catalog, they
-- may be able to guess the user. If this potential escalation path is a
-- concern, then Service Catalog recommends using @PrincipalType@ as @IAM@.
-- With this configuration, the @PrincipalARN@ must already exist in the
-- recipient account before it can be associated.
module Amazonka.ServiceCatalog.AssociatePrincipalWithPortfolio
  ( -- * Creating a Request
    AssociatePrincipalWithPortfolio (..),
    newAssociatePrincipalWithPortfolio,

    -- * Request Lenses
    associatePrincipalWithPortfolio_acceptLanguage,
    associatePrincipalWithPortfolio_portfolioId,
    associatePrincipalWithPortfolio_principalARN,
    associatePrincipalWithPortfolio_principalType,

    -- * Destructuring the Response
    AssociatePrincipalWithPortfolioResponse (..),
    newAssociatePrincipalWithPortfolioResponse,

    -- * Response Lenses
    associatePrincipalWithPortfolioResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newAssociatePrincipalWithPortfolio' smart constructor.
data AssociatePrincipalWithPortfolio = AssociatePrincipalWithPortfolio'
  { -- | The language code.
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The portfolio identifier.
    portfolioId :: Prelude.Text,
    -- | The ARN of the principal (user, role, or group). If the @PrincipalType@
    -- is @IAM@, the supported value is a fully defined
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM Amazon Resource Name (ARN)>.
    -- If the @PrincipalType@ is @IAM_PATTERN@, the supported value is an @IAM@
    -- ARN /without an AccountID/ in the following format:
    --
    -- /arn:partition:iam:::resource-type\/resource-id/
    --
    -- The ARN resource-id can be either:
    --
    -- -   A fully formed resource-id. For example,
    --     /arn:aws:iam:::role\/resource-name/ or
    --     /arn:aws:iam:::role\/resource-path\/resource-name/
    --
    -- -   A wildcard ARN. The wildcard ARN accepts @IAM_PATTERN@ values with a
    --     \"*\" or \"?\" in the resource-id segment of the ARN. For example
    --     /arn:partition:service:::resource-type\/resource-path\/resource-name/.
    --     The new symbols are exclusive to the __resource-path__ and
    --     __resource-name__ and cannot replace the __resource-type__ or other
    --     ARN values.
    --
    --     The ARN path and principal name allow unlimited wildcard characters.
    --
    -- Examples of an __acceptable__ wildcard ARN:
    --
    -- -   arn:aws:iam:::role\/ResourceName_*
    --
    -- -   arn:aws:iam:::role\/*\/ResourceName_?
    --
    -- Examples of an __unacceptable__ wildcard ARN:
    --
    -- -   arn:aws:iam:::*\/ResourceName
    --
    -- You can associate multiple @IAM_PATTERN@s even if the account has no
    -- principal with that name.
    --
    -- The \"?\" wildcard character matches zero or one of any character. This
    -- is similar to \".?\" in regular regex context. The \"*\" wildcard
    -- character matches any number of any characters. This is similar to
    -- \".*\" in regular regex context.
    --
    -- In the IAM Principal ARN format
    -- (/arn:partition:iam:::resource-type\/resource-path\/resource-name/),
    -- valid resource-type values include __user\/__, __group\/__, or
    -- __role\/__. The \"?\" and \"*\" characters are allowed only after the
    -- resource-type in the resource-id segment. You can use special characters
    -- anywhere within the resource-id.
    --
    -- The \"*\" character also matches the \"\/\" character, allowing paths to
    -- be formed /within/ the resource-id. For example,
    -- /arn:aws:iam:::role\/__*__\/ResourceName_?/ matches both
    -- /arn:aws:iam:::role\/pathA\/pathB\/ResourceName_1/ and
    -- /arn:aws:iam:::role\/pathA\/ResourceName_1/.
    principalARN :: Prelude.Text,
    -- | The principal type. The supported value is @IAM@ if you use a fully
    -- defined Amazon Resource Name (ARN), or @IAM_PATTERN@ if you use an ARN
    -- with no @accountID@, with or without wildcard characters.
    principalType :: PrincipalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePrincipalWithPortfolio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'associatePrincipalWithPortfolio_acceptLanguage' - The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'portfolioId', 'associatePrincipalWithPortfolio_portfolioId' - The portfolio identifier.
--
-- 'principalARN', 'associatePrincipalWithPortfolio_principalARN' - The ARN of the principal (user, role, or group). If the @PrincipalType@
-- is @IAM@, the supported value is a fully defined
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM Amazon Resource Name (ARN)>.
-- If the @PrincipalType@ is @IAM_PATTERN@, the supported value is an @IAM@
-- ARN /without an AccountID/ in the following format:
--
-- /arn:partition:iam:::resource-type\/resource-id/
--
-- The ARN resource-id can be either:
--
-- -   A fully formed resource-id. For example,
--     /arn:aws:iam:::role\/resource-name/ or
--     /arn:aws:iam:::role\/resource-path\/resource-name/
--
-- -   A wildcard ARN. The wildcard ARN accepts @IAM_PATTERN@ values with a
--     \"*\" or \"?\" in the resource-id segment of the ARN. For example
--     /arn:partition:service:::resource-type\/resource-path\/resource-name/.
--     The new symbols are exclusive to the __resource-path__ and
--     __resource-name__ and cannot replace the __resource-type__ or other
--     ARN values.
--
--     The ARN path and principal name allow unlimited wildcard characters.
--
-- Examples of an __acceptable__ wildcard ARN:
--
-- -   arn:aws:iam:::role\/ResourceName_*
--
-- -   arn:aws:iam:::role\/*\/ResourceName_?
--
-- Examples of an __unacceptable__ wildcard ARN:
--
-- -   arn:aws:iam:::*\/ResourceName
--
-- You can associate multiple @IAM_PATTERN@s even if the account has no
-- principal with that name.
--
-- The \"?\" wildcard character matches zero or one of any character. This
-- is similar to \".?\" in regular regex context. The \"*\" wildcard
-- character matches any number of any characters. This is similar to
-- \".*\" in regular regex context.
--
-- In the IAM Principal ARN format
-- (/arn:partition:iam:::resource-type\/resource-path\/resource-name/),
-- valid resource-type values include __user\/__, __group\/__, or
-- __role\/__. The \"?\" and \"*\" characters are allowed only after the
-- resource-type in the resource-id segment. You can use special characters
-- anywhere within the resource-id.
--
-- The \"*\" character also matches the \"\/\" character, allowing paths to
-- be formed /within/ the resource-id. For example,
-- /arn:aws:iam:::role\/__*__\/ResourceName_?/ matches both
-- /arn:aws:iam:::role\/pathA\/pathB\/ResourceName_1/ and
-- /arn:aws:iam:::role\/pathA\/ResourceName_1/.
--
-- 'principalType', 'associatePrincipalWithPortfolio_principalType' - The principal type. The supported value is @IAM@ if you use a fully
-- defined Amazon Resource Name (ARN), or @IAM_PATTERN@ if you use an ARN
-- with no @accountID@, with or without wildcard characters.
newAssociatePrincipalWithPortfolio ::
  -- | 'portfolioId'
  Prelude.Text ->
  -- | 'principalARN'
  Prelude.Text ->
  -- | 'principalType'
  PrincipalType ->
  AssociatePrincipalWithPortfolio
newAssociatePrincipalWithPortfolio
  pPortfolioId_
  pPrincipalARN_
  pPrincipalType_ =
    AssociatePrincipalWithPortfolio'
      { acceptLanguage =
          Prelude.Nothing,
        portfolioId = pPortfolioId_,
        principalARN = pPrincipalARN_,
        principalType = pPrincipalType_
      }

-- | The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
associatePrincipalWithPortfolio_acceptLanguage :: Lens.Lens' AssociatePrincipalWithPortfolio (Prelude.Maybe Prelude.Text)
associatePrincipalWithPortfolio_acceptLanguage = Lens.lens (\AssociatePrincipalWithPortfolio' {acceptLanguage} -> acceptLanguage) (\s@AssociatePrincipalWithPortfolio' {} a -> s {acceptLanguage = a} :: AssociatePrincipalWithPortfolio)

-- | The portfolio identifier.
associatePrincipalWithPortfolio_portfolioId :: Lens.Lens' AssociatePrincipalWithPortfolio Prelude.Text
associatePrincipalWithPortfolio_portfolioId = Lens.lens (\AssociatePrincipalWithPortfolio' {portfolioId} -> portfolioId) (\s@AssociatePrincipalWithPortfolio' {} a -> s {portfolioId = a} :: AssociatePrincipalWithPortfolio)

-- | The ARN of the principal (user, role, or group). If the @PrincipalType@
-- is @IAM@, the supported value is a fully defined
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html#identifiers-arns IAM Amazon Resource Name (ARN)>.
-- If the @PrincipalType@ is @IAM_PATTERN@, the supported value is an @IAM@
-- ARN /without an AccountID/ in the following format:
--
-- /arn:partition:iam:::resource-type\/resource-id/
--
-- The ARN resource-id can be either:
--
-- -   A fully formed resource-id. For example,
--     /arn:aws:iam:::role\/resource-name/ or
--     /arn:aws:iam:::role\/resource-path\/resource-name/
--
-- -   A wildcard ARN. The wildcard ARN accepts @IAM_PATTERN@ values with a
--     \"*\" or \"?\" in the resource-id segment of the ARN. For example
--     /arn:partition:service:::resource-type\/resource-path\/resource-name/.
--     The new symbols are exclusive to the __resource-path__ and
--     __resource-name__ and cannot replace the __resource-type__ or other
--     ARN values.
--
--     The ARN path and principal name allow unlimited wildcard characters.
--
-- Examples of an __acceptable__ wildcard ARN:
--
-- -   arn:aws:iam:::role\/ResourceName_*
--
-- -   arn:aws:iam:::role\/*\/ResourceName_?
--
-- Examples of an __unacceptable__ wildcard ARN:
--
-- -   arn:aws:iam:::*\/ResourceName
--
-- You can associate multiple @IAM_PATTERN@s even if the account has no
-- principal with that name.
--
-- The \"?\" wildcard character matches zero or one of any character. This
-- is similar to \".?\" in regular regex context. The \"*\" wildcard
-- character matches any number of any characters. This is similar to
-- \".*\" in regular regex context.
--
-- In the IAM Principal ARN format
-- (/arn:partition:iam:::resource-type\/resource-path\/resource-name/),
-- valid resource-type values include __user\/__, __group\/__, or
-- __role\/__. The \"?\" and \"*\" characters are allowed only after the
-- resource-type in the resource-id segment. You can use special characters
-- anywhere within the resource-id.
--
-- The \"*\" character also matches the \"\/\" character, allowing paths to
-- be formed /within/ the resource-id. For example,
-- /arn:aws:iam:::role\/__*__\/ResourceName_?/ matches both
-- /arn:aws:iam:::role\/pathA\/pathB\/ResourceName_1/ and
-- /arn:aws:iam:::role\/pathA\/ResourceName_1/.
associatePrincipalWithPortfolio_principalARN :: Lens.Lens' AssociatePrincipalWithPortfolio Prelude.Text
associatePrincipalWithPortfolio_principalARN = Lens.lens (\AssociatePrincipalWithPortfolio' {principalARN} -> principalARN) (\s@AssociatePrincipalWithPortfolio' {} a -> s {principalARN = a} :: AssociatePrincipalWithPortfolio)

-- | The principal type. The supported value is @IAM@ if you use a fully
-- defined Amazon Resource Name (ARN), or @IAM_PATTERN@ if you use an ARN
-- with no @accountID@, with or without wildcard characters.
associatePrincipalWithPortfolio_principalType :: Lens.Lens' AssociatePrincipalWithPortfolio PrincipalType
associatePrincipalWithPortfolio_principalType = Lens.lens (\AssociatePrincipalWithPortfolio' {principalType} -> principalType) (\s@AssociatePrincipalWithPortfolio' {} a -> s {principalType = a} :: AssociatePrincipalWithPortfolio)

instance
  Core.AWSRequest
    AssociatePrincipalWithPortfolio
  where
  type
    AWSResponse AssociatePrincipalWithPortfolio =
      AssociatePrincipalWithPortfolioResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociatePrincipalWithPortfolioResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociatePrincipalWithPortfolio
  where
  hashWithSalt
    _salt
    AssociatePrincipalWithPortfolio' {..} =
      _salt
        `Prelude.hashWithSalt` acceptLanguage
        `Prelude.hashWithSalt` portfolioId
        `Prelude.hashWithSalt` principalARN
        `Prelude.hashWithSalt` principalType

instance
  Prelude.NFData
    AssociatePrincipalWithPortfolio
  where
  rnf AssociatePrincipalWithPortfolio' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf portfolioId
      `Prelude.seq` Prelude.rnf principalARN
      `Prelude.seq` Prelude.rnf principalType

instance
  Data.ToHeaders
    AssociatePrincipalWithPortfolio
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.AssociatePrincipalWithPortfolio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociatePrincipalWithPortfolio where
  toJSON AssociatePrincipalWithPortfolio' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("PortfolioId" Data..= portfolioId),
            Prelude.Just ("PrincipalARN" Data..= principalARN),
            Prelude.Just
              ("PrincipalType" Data..= principalType)
          ]
      )

instance Data.ToPath AssociatePrincipalWithPortfolio where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociatePrincipalWithPortfolio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociatePrincipalWithPortfolioResponse' smart constructor.
data AssociatePrincipalWithPortfolioResponse = AssociatePrincipalWithPortfolioResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatePrincipalWithPortfolioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associatePrincipalWithPortfolioResponse_httpStatus' - The response's http status code.
newAssociatePrincipalWithPortfolioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociatePrincipalWithPortfolioResponse
newAssociatePrincipalWithPortfolioResponse
  pHttpStatus_ =
    AssociatePrincipalWithPortfolioResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associatePrincipalWithPortfolioResponse_httpStatus :: Lens.Lens' AssociatePrincipalWithPortfolioResponse Prelude.Int
associatePrincipalWithPortfolioResponse_httpStatus = Lens.lens (\AssociatePrincipalWithPortfolioResponse' {httpStatus} -> httpStatus) (\s@AssociatePrincipalWithPortfolioResponse' {} a -> s {httpStatus = a} :: AssociatePrincipalWithPortfolioResponse)

instance
  Prelude.NFData
    AssociatePrincipalWithPortfolioResponse
  where
  rnf AssociatePrincipalWithPortfolioResponse' {..} =
    Prelude.rnf httpStatus
