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
-- Module      : Amazonka.CustomerProfiles.UpdateCalculatedAttributeDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing calculated attribute definition. When updating the
-- Conditions, note that increasing the date range of a calculated
-- attribute will not trigger inclusion of historical data greater than the
-- current date range.
module Amazonka.CustomerProfiles.UpdateCalculatedAttributeDefinition
  ( -- * Creating a Request
    UpdateCalculatedAttributeDefinition (..),
    newUpdateCalculatedAttributeDefinition,

    -- * Request Lenses
    updateCalculatedAttributeDefinition_conditions,
    updateCalculatedAttributeDefinition_description,
    updateCalculatedAttributeDefinition_displayName,
    updateCalculatedAttributeDefinition_domainName,
    updateCalculatedAttributeDefinition_calculatedAttributeName,

    -- * Destructuring the Response
    UpdateCalculatedAttributeDefinitionResponse (..),
    newUpdateCalculatedAttributeDefinitionResponse,

    -- * Response Lenses
    updateCalculatedAttributeDefinitionResponse_attributeDetails,
    updateCalculatedAttributeDefinitionResponse_calculatedAttributeName,
    updateCalculatedAttributeDefinitionResponse_conditions,
    updateCalculatedAttributeDefinitionResponse_createdAt,
    updateCalculatedAttributeDefinitionResponse_description,
    updateCalculatedAttributeDefinitionResponse_displayName,
    updateCalculatedAttributeDefinitionResponse_lastUpdatedAt,
    updateCalculatedAttributeDefinitionResponse_statistic,
    updateCalculatedAttributeDefinitionResponse_tags,
    updateCalculatedAttributeDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCalculatedAttributeDefinition' smart constructor.
data UpdateCalculatedAttributeDefinition = UpdateCalculatedAttributeDefinition'
  { -- | The conditions including range, object count, and threshold for the
    -- calculated attribute.
    conditions :: Prelude.Maybe Conditions,
    -- | The description of the calculated attribute.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the calculated attribute.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCalculatedAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditions', 'updateCalculatedAttributeDefinition_conditions' - The conditions including range, object count, and threshold for the
-- calculated attribute.
--
-- 'description', 'updateCalculatedAttributeDefinition_description' - The description of the calculated attribute.
--
-- 'displayName', 'updateCalculatedAttributeDefinition_displayName' - The display name of the calculated attribute.
--
-- 'domainName', 'updateCalculatedAttributeDefinition_domainName' - The unique name of the domain.
--
-- 'calculatedAttributeName', 'updateCalculatedAttributeDefinition_calculatedAttributeName' - The unique name of the calculated attribute.
newUpdateCalculatedAttributeDefinition ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'calculatedAttributeName'
  Prelude.Text ->
  UpdateCalculatedAttributeDefinition
newUpdateCalculatedAttributeDefinition
  pDomainName_
  pCalculatedAttributeName_ =
    UpdateCalculatedAttributeDefinition'
      { conditions =
          Prelude.Nothing,
        description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        domainName = pDomainName_,
        calculatedAttributeName =
          pCalculatedAttributeName_
      }

-- | The conditions including range, object count, and threshold for the
-- calculated attribute.
updateCalculatedAttributeDefinition_conditions :: Lens.Lens' UpdateCalculatedAttributeDefinition (Prelude.Maybe Conditions)
updateCalculatedAttributeDefinition_conditions = Lens.lens (\UpdateCalculatedAttributeDefinition' {conditions} -> conditions) (\s@UpdateCalculatedAttributeDefinition' {} a -> s {conditions = a} :: UpdateCalculatedAttributeDefinition)

-- | The description of the calculated attribute.
updateCalculatedAttributeDefinition_description :: Lens.Lens' UpdateCalculatedAttributeDefinition (Prelude.Maybe Prelude.Text)
updateCalculatedAttributeDefinition_description = Lens.lens (\UpdateCalculatedAttributeDefinition' {description} -> description) (\s@UpdateCalculatedAttributeDefinition' {} a -> s {description = a} :: UpdateCalculatedAttributeDefinition)

-- | The display name of the calculated attribute.
updateCalculatedAttributeDefinition_displayName :: Lens.Lens' UpdateCalculatedAttributeDefinition (Prelude.Maybe Prelude.Text)
updateCalculatedAttributeDefinition_displayName = Lens.lens (\UpdateCalculatedAttributeDefinition' {displayName} -> displayName) (\s@UpdateCalculatedAttributeDefinition' {} a -> s {displayName = a} :: UpdateCalculatedAttributeDefinition)

-- | The unique name of the domain.
updateCalculatedAttributeDefinition_domainName :: Lens.Lens' UpdateCalculatedAttributeDefinition Prelude.Text
updateCalculatedAttributeDefinition_domainName = Lens.lens (\UpdateCalculatedAttributeDefinition' {domainName} -> domainName) (\s@UpdateCalculatedAttributeDefinition' {} a -> s {domainName = a} :: UpdateCalculatedAttributeDefinition)

-- | The unique name of the calculated attribute.
updateCalculatedAttributeDefinition_calculatedAttributeName :: Lens.Lens' UpdateCalculatedAttributeDefinition Prelude.Text
updateCalculatedAttributeDefinition_calculatedAttributeName = Lens.lens (\UpdateCalculatedAttributeDefinition' {calculatedAttributeName} -> calculatedAttributeName) (\s@UpdateCalculatedAttributeDefinition' {} a -> s {calculatedAttributeName = a} :: UpdateCalculatedAttributeDefinition)

instance
  Core.AWSRequest
    UpdateCalculatedAttributeDefinition
  where
  type
    AWSResponse UpdateCalculatedAttributeDefinition =
      UpdateCalculatedAttributeDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCalculatedAttributeDefinitionResponse'
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
    UpdateCalculatedAttributeDefinition
  where
  hashWithSalt
    _salt
    UpdateCalculatedAttributeDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` conditions
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` displayName
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` calculatedAttributeName

instance
  Prelude.NFData
    UpdateCalculatedAttributeDefinition
  where
  rnf UpdateCalculatedAttributeDefinition' {..} =
    Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf calculatedAttributeName

instance
  Data.ToHeaders
    UpdateCalculatedAttributeDefinition
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

instance
  Data.ToJSON
    UpdateCalculatedAttributeDefinition
  where
  toJSON UpdateCalculatedAttributeDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Conditions" Data..=) Prelude.<$> conditions,
            ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName
          ]
      )

instance
  Data.ToPath
    UpdateCalculatedAttributeDefinition
  where
  toPath UpdateCalculatedAttributeDefinition' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/calculated-attributes/",
        Data.toBS calculatedAttributeName
      ]

instance
  Data.ToQuery
    UpdateCalculatedAttributeDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCalculatedAttributeDefinitionResponse' smart constructor.
data UpdateCalculatedAttributeDefinitionResponse = UpdateCalculatedAttributeDefinitionResponse'
  { -- | The mathematical expression and a list of attribute items specified in
    -- that expression.
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
-- Create a value of 'UpdateCalculatedAttributeDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeDetails', 'updateCalculatedAttributeDefinitionResponse_attributeDetails' - The mathematical expression and a list of attribute items specified in
-- that expression.
--
-- 'calculatedAttributeName', 'updateCalculatedAttributeDefinitionResponse_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'conditions', 'updateCalculatedAttributeDefinitionResponse_conditions' - The conditions including range, object count, and threshold for the
-- calculated attribute.
--
-- 'createdAt', 'updateCalculatedAttributeDefinitionResponse_createdAt' - The timestamp of when the calculated attribute definition was created.
--
-- 'description', 'updateCalculatedAttributeDefinitionResponse_description' - The description of the calculated attribute.
--
-- 'displayName', 'updateCalculatedAttributeDefinitionResponse_displayName' - The display name of the calculated attribute.
--
-- 'lastUpdatedAt', 'updateCalculatedAttributeDefinitionResponse_lastUpdatedAt' - The timestamp of when the calculated attribute definition was most
-- recently edited.
--
-- 'statistic', 'updateCalculatedAttributeDefinitionResponse_statistic' - The aggregation operation to perform for the calculated attribute.
--
-- 'tags', 'updateCalculatedAttributeDefinitionResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'updateCalculatedAttributeDefinitionResponse_httpStatus' - The response's http status code.
newUpdateCalculatedAttributeDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCalculatedAttributeDefinitionResponse
newUpdateCalculatedAttributeDefinitionResponse
  pHttpStatus_ =
    UpdateCalculatedAttributeDefinitionResponse'
      { attributeDetails =
          Prelude.Nothing,
        calculatedAttributeName =
          Prelude.Nothing,
        conditions = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        lastUpdatedAt =
          Prelude.Nothing,
        statistic = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The mathematical expression and a list of attribute items specified in
-- that expression.
updateCalculatedAttributeDefinitionResponse_attributeDetails :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe AttributeDetails)
updateCalculatedAttributeDefinitionResponse_attributeDetails = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {attributeDetails} -> attributeDetails) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {attributeDetails = a} :: UpdateCalculatedAttributeDefinitionResponse)

-- | The unique name of the calculated attribute.
updateCalculatedAttributeDefinitionResponse_calculatedAttributeName :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
updateCalculatedAttributeDefinitionResponse_calculatedAttributeName = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {calculatedAttributeName} -> calculatedAttributeName) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {calculatedAttributeName = a} :: UpdateCalculatedAttributeDefinitionResponse)

-- | The conditions including range, object count, and threshold for the
-- calculated attribute.
updateCalculatedAttributeDefinitionResponse_conditions :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Conditions)
updateCalculatedAttributeDefinitionResponse_conditions = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {conditions} -> conditions) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {conditions = a} :: UpdateCalculatedAttributeDefinitionResponse)

-- | The timestamp of when the calculated attribute definition was created.
updateCalculatedAttributeDefinitionResponse_createdAt :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
updateCalculatedAttributeDefinitionResponse_createdAt = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {createdAt} -> createdAt) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {createdAt = a} :: UpdateCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the calculated attribute.
updateCalculatedAttributeDefinitionResponse_description :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
updateCalculatedAttributeDefinitionResponse_description = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {description} -> description) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {description = a} :: UpdateCalculatedAttributeDefinitionResponse)

-- | The display name of the calculated attribute.
updateCalculatedAttributeDefinitionResponse_displayName :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
updateCalculatedAttributeDefinitionResponse_displayName = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {displayName} -> displayName) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {displayName = a} :: UpdateCalculatedAttributeDefinitionResponse)

-- | The timestamp of when the calculated attribute definition was most
-- recently edited.
updateCalculatedAttributeDefinitionResponse_lastUpdatedAt :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
updateCalculatedAttributeDefinitionResponse_lastUpdatedAt = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {lastUpdatedAt = a} :: UpdateCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The aggregation operation to perform for the calculated attribute.
updateCalculatedAttributeDefinitionResponse_statistic :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe Statistic)
updateCalculatedAttributeDefinitionResponse_statistic = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {statistic} -> statistic) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {statistic = a} :: UpdateCalculatedAttributeDefinitionResponse)

-- | The tags used to organize, track, or control access for this resource.
updateCalculatedAttributeDefinitionResponse_tags :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateCalculatedAttributeDefinitionResponse_tags = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {tags} -> tags) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {tags = a} :: UpdateCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateCalculatedAttributeDefinitionResponse_httpStatus :: Lens.Lens' UpdateCalculatedAttributeDefinitionResponse Prelude.Int
updateCalculatedAttributeDefinitionResponse_httpStatus = Lens.lens (\UpdateCalculatedAttributeDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateCalculatedAttributeDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateCalculatedAttributeDefinitionResponse)

instance
  Prelude.NFData
    UpdateCalculatedAttributeDefinitionResponse
  where
  rnf UpdateCalculatedAttributeDefinitionResponse' {..} =
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
