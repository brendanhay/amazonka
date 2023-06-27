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
-- Module      : Amazonka.CustomerProfiles.GetCalculatedAttributeDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information on a calculated attribute definition for
-- Customer Profiles.
module Amazonka.CustomerProfiles.GetCalculatedAttributeDefinition
  ( -- * Creating a Request
    GetCalculatedAttributeDefinition (..),
    newGetCalculatedAttributeDefinition,

    -- * Request Lenses
    getCalculatedAttributeDefinition_domainName,
    getCalculatedAttributeDefinition_calculatedAttributeName,

    -- * Destructuring the Response
    GetCalculatedAttributeDefinitionResponse (..),
    newGetCalculatedAttributeDefinitionResponse,

    -- * Response Lenses
    getCalculatedAttributeDefinitionResponse_attributeDetails,
    getCalculatedAttributeDefinitionResponse_calculatedAttributeName,
    getCalculatedAttributeDefinitionResponse_conditions,
    getCalculatedAttributeDefinitionResponse_createdAt,
    getCalculatedAttributeDefinitionResponse_description,
    getCalculatedAttributeDefinitionResponse_displayName,
    getCalculatedAttributeDefinitionResponse_lastUpdatedAt,
    getCalculatedAttributeDefinitionResponse_statistic,
    getCalculatedAttributeDefinitionResponse_tags,
    getCalculatedAttributeDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCalculatedAttributeDefinition' smart constructor.
data GetCalculatedAttributeDefinition = GetCalculatedAttributeDefinition'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculatedAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getCalculatedAttributeDefinition_domainName' - The unique name of the domain.
--
-- 'calculatedAttributeName', 'getCalculatedAttributeDefinition_calculatedAttributeName' - The unique name of the calculated attribute.
newGetCalculatedAttributeDefinition ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'calculatedAttributeName'
  Prelude.Text ->
  GetCalculatedAttributeDefinition
newGetCalculatedAttributeDefinition
  pDomainName_
  pCalculatedAttributeName_ =
    GetCalculatedAttributeDefinition'
      { domainName =
          pDomainName_,
        calculatedAttributeName =
          pCalculatedAttributeName_
      }

-- | The unique name of the domain.
getCalculatedAttributeDefinition_domainName :: Lens.Lens' GetCalculatedAttributeDefinition Prelude.Text
getCalculatedAttributeDefinition_domainName = Lens.lens (\GetCalculatedAttributeDefinition' {domainName} -> domainName) (\s@GetCalculatedAttributeDefinition' {} a -> s {domainName = a} :: GetCalculatedAttributeDefinition)

-- | The unique name of the calculated attribute.
getCalculatedAttributeDefinition_calculatedAttributeName :: Lens.Lens' GetCalculatedAttributeDefinition Prelude.Text
getCalculatedAttributeDefinition_calculatedAttributeName = Lens.lens (\GetCalculatedAttributeDefinition' {calculatedAttributeName} -> calculatedAttributeName) (\s@GetCalculatedAttributeDefinition' {} a -> s {calculatedAttributeName = a} :: GetCalculatedAttributeDefinition)

instance
  Core.AWSRequest
    GetCalculatedAttributeDefinition
  where
  type
    AWSResponse GetCalculatedAttributeDefinition =
      GetCalculatedAttributeDefinitionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCalculatedAttributeDefinitionResponse'
            Prelude.<$> (x Data..?> "AttributeDetails")
            Prelude.<*> (x Data..?> "CalculatedAttributeName")
            Prelude.<*> (x Data..?> "Conditions")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> (x Data..?> "Statistic")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCalculatedAttributeDefinition
  where
  hashWithSalt
    _salt
    GetCalculatedAttributeDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` calculatedAttributeName

instance
  Prelude.NFData
    GetCalculatedAttributeDefinition
  where
  rnf GetCalculatedAttributeDefinition' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf calculatedAttributeName

instance
  Data.ToHeaders
    GetCalculatedAttributeDefinition
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

instance Data.ToPath GetCalculatedAttributeDefinition where
  toPath GetCalculatedAttributeDefinition' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/calculated-attributes/",
        Data.toBS calculatedAttributeName
      ]

instance
  Data.ToQuery
    GetCalculatedAttributeDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCalculatedAttributeDefinitionResponse' smart constructor.
data GetCalculatedAttributeDefinitionResponse = GetCalculatedAttributeDefinitionResponse'
  { -- | Mathematical expression and a list of attribute items specified in that
    -- expression.
    attributeDetails :: Prelude.Maybe AttributeDetails,
    -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The conditions including range, object count, and threshold for the
    -- calculated attribute.
    conditions :: Prelude.Maybe Conditions,
    -- | The timestamp of when the calculated attribute definition was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description of the calculated attribute.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the calculated attribute.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the calculated attribute definition was most
    -- recently edited.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The aggregation operation to perform for the calculated attribute.
    statistic :: Prelude.Maybe Statistic,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalculatedAttributeDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeDetails', 'getCalculatedAttributeDefinitionResponse_attributeDetails' - Mathematical expression and a list of attribute items specified in that
-- expression.
--
-- 'calculatedAttributeName', 'getCalculatedAttributeDefinitionResponse_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'conditions', 'getCalculatedAttributeDefinitionResponse_conditions' - The conditions including range, object count, and threshold for the
-- calculated attribute.
--
-- 'createdAt', 'getCalculatedAttributeDefinitionResponse_createdAt' - The timestamp of when the calculated attribute definition was created.
--
-- 'description', 'getCalculatedAttributeDefinitionResponse_description' - The description of the calculated attribute.
--
-- 'displayName', 'getCalculatedAttributeDefinitionResponse_displayName' - The display name of the calculated attribute.
--
-- 'lastUpdatedAt', 'getCalculatedAttributeDefinitionResponse_lastUpdatedAt' - The timestamp of when the calculated attribute definition was most
-- recently edited.
--
-- 'statistic', 'getCalculatedAttributeDefinitionResponse_statistic' - The aggregation operation to perform for the calculated attribute.
--
-- 'tags', 'getCalculatedAttributeDefinitionResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'getCalculatedAttributeDefinitionResponse_httpStatus' - The response's http status code.
newGetCalculatedAttributeDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCalculatedAttributeDefinitionResponse
newGetCalculatedAttributeDefinitionResponse
  pHttpStatus_ =
    GetCalculatedAttributeDefinitionResponse'
      { attributeDetails =
          Prelude.Nothing,
        calculatedAttributeName =
          Prelude.Nothing,
        conditions = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        statistic = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Mathematical expression and a list of attribute items specified in that
-- expression.
getCalculatedAttributeDefinitionResponse_attributeDetails :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe AttributeDetails)
getCalculatedAttributeDefinitionResponse_attributeDetails = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {attributeDetails} -> attributeDetails) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {attributeDetails = a} :: GetCalculatedAttributeDefinitionResponse)

-- | The unique name of the calculated attribute.
getCalculatedAttributeDefinitionResponse_calculatedAttributeName :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeDefinitionResponse_calculatedAttributeName = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {calculatedAttributeName} -> calculatedAttributeName) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {calculatedAttributeName = a} :: GetCalculatedAttributeDefinitionResponse)

-- | The conditions including range, object count, and threshold for the
-- calculated attribute.
getCalculatedAttributeDefinitionResponse_conditions :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Conditions)
getCalculatedAttributeDefinitionResponse_conditions = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {conditions} -> conditions) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {conditions = a} :: GetCalculatedAttributeDefinitionResponse)

-- | The timestamp of when the calculated attribute definition was created.
getCalculatedAttributeDefinitionResponse_createdAt :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
getCalculatedAttributeDefinitionResponse_createdAt = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {createdAt} -> createdAt) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {createdAt = a} :: GetCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the calculated attribute.
getCalculatedAttributeDefinitionResponse_description :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeDefinitionResponse_description = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {description} -> description) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {description = a} :: GetCalculatedAttributeDefinitionResponse)

-- | The display name of the calculated attribute.
getCalculatedAttributeDefinitionResponse_displayName :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
getCalculatedAttributeDefinitionResponse_displayName = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {displayName} -> displayName) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {displayName = a} :: GetCalculatedAttributeDefinitionResponse)

-- | The timestamp of when the calculated attribute definition was most
-- recently edited.
getCalculatedAttributeDefinitionResponse_lastUpdatedAt :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
getCalculatedAttributeDefinitionResponse_lastUpdatedAt = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {lastUpdatedAt = a} :: GetCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The aggregation operation to perform for the calculated attribute.
getCalculatedAttributeDefinitionResponse_statistic :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe Statistic)
getCalculatedAttributeDefinitionResponse_statistic = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {statistic} -> statistic) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {statistic = a} :: GetCalculatedAttributeDefinitionResponse)

-- | The tags used to organize, track, or control access for this resource.
getCalculatedAttributeDefinitionResponse_tags :: Lens.Lens' GetCalculatedAttributeDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getCalculatedAttributeDefinitionResponse_tags = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {tags} -> tags) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {tags = a} :: GetCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCalculatedAttributeDefinitionResponse_httpStatus :: Lens.Lens' GetCalculatedAttributeDefinitionResponse Prelude.Int
getCalculatedAttributeDefinitionResponse_httpStatus = Lens.lens (\GetCalculatedAttributeDefinitionResponse' {httpStatus} -> httpStatus) (\s@GetCalculatedAttributeDefinitionResponse' {} a -> s {httpStatus = a} :: GetCalculatedAttributeDefinitionResponse)

instance
  Prelude.NFData
    GetCalculatedAttributeDefinitionResponse
  where
  rnf GetCalculatedAttributeDefinitionResponse' {..} =
    Prelude.rnf attributeDetails
      `Prelude.seq` Prelude.rnf calculatedAttributeName
      `Prelude.seq` Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf statistic
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
