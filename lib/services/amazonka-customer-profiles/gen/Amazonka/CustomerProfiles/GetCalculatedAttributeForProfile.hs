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
-- Module      : Amazonka.CustomerProfiles.GetCalculatedAttributeForProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a calculated attribute for a customer profile.
module Amazonka.CustomerProfiles.GetCalculatedAttributeForProfile
  ( -- * Creating a Request
    GetCalculatedAttributeForProfile (..),
    newGetCalculatedAttributeForProfile,

    -- * Request Lenses
    getCalculatedAttributeForProfile_domainName,
    getCalculatedAttributeForProfile_profileId,
    getCalculatedAttributeForProfile_calculatedAttributeName,

    -- * Destructuring the Response
    GetCalculatedAttributeForProfileResponse (..),
    newGetCalculatedAttributeForProfileResponse,

    -- * Response Lenses
    getCalculatedAttributeForProfileResponse_calculatedAttributeName,
    getCalculatedAttributeForProfileResponse_displayName,
    getCalculatedAttributeForProfileResponse_isDataPartial,
    getCalculatedAttributeForProfileResponse_value,
    getCalculatedAttributeForProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCalculatedAttributeForProfile' smart constructor.
data GetCalculatedAttributeForProfile = GetCalculatedAttributeForProfile'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text,
    -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculatedAttributeForProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getCalculatedAttributeForProfile_domainName' - The unique name of the domain.
--
-- 'profileId', 'getCalculatedAttributeForProfile_profileId' - The unique identifier of a customer profile.
--
-- 'calculatedAttributeName', 'getCalculatedAttributeForProfile_calculatedAttributeName' - The unique name of the calculated attribute.
newGetCalculatedAttributeForProfile ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'profileId'
  Prelude.Text ->
  -- | 'calculatedAttributeName'
  Prelude.Text ->
  GetCalculatedAttributeForProfile
newGetCalculatedAttributeForProfile
  pDomainName_
  pProfileId_
  pCalculatedAttributeName_ =
    GetCalculatedAttributeForProfile'
      { domainName =
          pDomainName_,
        profileId = pProfileId_,
        calculatedAttributeName =
          pCalculatedAttributeName_
      }

-- | The unique name of the domain.
getCalculatedAttributeForProfile_domainName :: Lens.Lens' GetCalculatedAttributeForProfile Prelude.Text
getCalculatedAttributeForProfile_domainName = Lens.lens (\GetCalculatedAttributeForProfile' {domainName} -> domainName) (\s@GetCalculatedAttributeForProfile' {} a -> s {domainName = a} :: GetCalculatedAttributeForProfile)

-- | The unique identifier of a customer profile.
getCalculatedAttributeForProfile_profileId :: Lens.Lens' GetCalculatedAttributeForProfile Prelude.Text
getCalculatedAttributeForProfile_profileId = Lens.lens (\GetCalculatedAttributeForProfile' {profileId} -> profileId) (\s@GetCalculatedAttributeForProfile' {} a -> s {profileId = a} :: GetCalculatedAttributeForProfile)

-- | The unique name of the calculated attribute.
getCalculatedAttributeForProfile_calculatedAttributeName :: Lens.Lens' GetCalculatedAttributeForProfile Prelude.Text
getCalculatedAttributeForProfile_calculatedAttributeName = Lens.lens (\GetCalculatedAttributeForProfile' {calculatedAttributeName} -> calculatedAttributeName) (\s@GetCalculatedAttributeForProfile' {} a -> s {calculatedAttributeName = a} :: GetCalculatedAttributeForProfile)

instance
  Core.AWSRequest
    GetCalculatedAttributeForProfile
  where
  type
    AWSResponse GetCalculatedAttributeForProfile =
      GetCalculatedAttributeForProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCalculatedAttributeForProfileResponse'
            Prelude.<$> (x Data..?> "CalculatedAttributeName")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "IsDataPartial")
            Prelude.<*> (x Data..?> "Value")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCalculatedAttributeForProfile
  where
  hashWithSalt
    _salt
    GetCalculatedAttributeForProfile' {..} =
      _salt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` profileId
        `Prelude.hashWithSalt` calculatedAttributeName

instance
  Prelude.NFData
    GetCalculatedAttributeForProfile
  where
  rnf GetCalculatedAttributeForProfile' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf calculatedAttributeName

instance
  Data.ToHeaders
    GetCalculatedAttributeForProfile
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCalculatedAttributeForProfile where
  toPath GetCalculatedAttributeForProfile' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profile/",
        Data.toBS profileId,
        "/calculated-attributes/",
        Data.toBS calculatedAttributeName
      ]

instance
  Data.ToQuery
    GetCalculatedAttributeForProfile
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCalculatedAttributeForProfileResponse' smart constructor.
data GetCalculatedAttributeForProfileResponse = GetCalculatedAttributeForProfileResponse'
  { -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The display name of the calculated attribute.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the calculated attribute’s value is based on partial
    -- data. If data is partial, it is set to true.
    isDataPartial :: Prelude.Maybe Prelude.Text,
    -- | The value of the calculated attribute.
    value :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculatedAttributeForProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculatedAttributeName', 'getCalculatedAttributeForProfileResponse_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'displayName', 'getCalculatedAttributeForProfileResponse_displayName' - The display name of the calculated attribute.
--
-- 'isDataPartial', 'getCalculatedAttributeForProfileResponse_isDataPartial' - Indicates whether the calculated attribute’s value is based on partial
-- data. If data is partial, it is set to true.
--
-- 'value', 'getCalculatedAttributeForProfileResponse_value' - The value of the calculated attribute.
--
-- 'httpStatus', 'getCalculatedAttributeForProfileResponse_httpStatus' - The response's http status code.
newGetCalculatedAttributeForProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCalculatedAttributeForProfileResponse
newGetCalculatedAttributeForProfileResponse
  pHttpStatus_ =
    GetCalculatedAttributeForProfileResponse'
      { calculatedAttributeName =
          Prelude.Nothing,
        displayName = Prelude.Nothing,
        isDataPartial = Prelude.Nothing,
        value = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The unique name of the calculated attribute.
getCalculatedAttributeForProfileResponse_calculatedAttributeName :: Lens.Lens' GetCalculatedAttributeForProfileResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeForProfileResponse_calculatedAttributeName = Lens.lens (\GetCalculatedAttributeForProfileResponse' {calculatedAttributeName} -> calculatedAttributeName) (\s@GetCalculatedAttributeForProfileResponse' {} a -> s {calculatedAttributeName = a} :: GetCalculatedAttributeForProfileResponse)

-- | The display name of the calculated attribute.
getCalculatedAttributeForProfileResponse_displayName :: Lens.Lens' GetCalculatedAttributeForProfileResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeForProfileResponse_displayName = Lens.lens (\GetCalculatedAttributeForProfileResponse' {displayName} -> displayName) (\s@GetCalculatedAttributeForProfileResponse' {} a -> s {displayName = a} :: GetCalculatedAttributeForProfileResponse)

-- | Indicates whether the calculated attribute’s value is based on partial
-- data. If data is partial, it is set to true.
getCalculatedAttributeForProfileResponse_isDataPartial :: Lens.Lens' GetCalculatedAttributeForProfileResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeForProfileResponse_isDataPartial = Lens.lens (\GetCalculatedAttributeForProfileResponse' {isDataPartial} -> isDataPartial) (\s@GetCalculatedAttributeForProfileResponse' {} a -> s {isDataPartial = a} :: GetCalculatedAttributeForProfileResponse)

-- | The value of the calculated attribute.
getCalculatedAttributeForProfileResponse_value :: Lens.Lens' GetCalculatedAttributeForProfileResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeForProfileResponse_value = Lens.lens (\GetCalculatedAttributeForProfileResponse' {value} -> value) (\s@GetCalculatedAttributeForProfileResponse' {} a -> s {value = a} :: GetCalculatedAttributeForProfileResponse)

-- | The response's http status code.
getCalculatedAttributeForProfileResponse_httpStatus :: Lens.Lens' GetCalculatedAttributeForProfileResponse Prelude.Int
getCalculatedAttributeForProfileResponse_httpStatus = Lens.lens (\GetCalculatedAttributeForProfileResponse' {httpStatus} -> httpStatus) (\s@GetCalculatedAttributeForProfileResponse' {} a -> s {httpStatus = a} :: GetCalculatedAttributeForProfileResponse)

instance
  Prelude.NFData
    GetCalculatedAttributeForProfileResponse
  where
  rnf GetCalculatedAttributeForProfileResponse' {..} =
    Prelude.rnf calculatedAttributeName
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf isDataPartial
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf httpStatus
