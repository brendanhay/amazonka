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
-- Module      : Amazonka.CustomerProfiles.CreateCalculatedAttributeDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new calculated attribute definition. After creation, new
-- object data ingested into Customer Profiles will be included in the
-- calculated attribute, which can be retrieved for a profile using the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_GetCalculatedAttributeForProfile.html GetCalculatedAttributeForProfile>
-- API. Defining a calculated attribute makes it available for all profiles
-- within a domain. Each calculated attribute can only reference one
-- @ObjectType@ and at most, two fields from that @ObjectType@.
module Amazonka.CustomerProfiles.CreateCalculatedAttributeDefinition
  ( -- * Creating a Request
    CreateCalculatedAttributeDefinition (..),
    newCreateCalculatedAttributeDefinition,

    -- * Request Lenses
    createCalculatedAttributeDefinition_conditions,
    createCalculatedAttributeDefinition_description,
    createCalculatedAttributeDefinition_displayName,
    createCalculatedAttributeDefinition_tags,
    createCalculatedAttributeDefinition_domainName,
    createCalculatedAttributeDefinition_calculatedAttributeName,
    createCalculatedAttributeDefinition_attributeDetails,
    createCalculatedAttributeDefinition_statistic,

    -- * Destructuring the Response
    CreateCalculatedAttributeDefinitionResponse (..),
    newCreateCalculatedAttributeDefinitionResponse,

    -- * Response Lenses
    createCalculatedAttributeDefinitionResponse_attributeDetails,
    createCalculatedAttributeDefinitionResponse_calculatedAttributeName,
    createCalculatedAttributeDefinitionResponse_conditions,
    createCalculatedAttributeDefinitionResponse_createdAt,
    createCalculatedAttributeDefinitionResponse_description,
    createCalculatedAttributeDefinitionResponse_displayName,
    createCalculatedAttributeDefinitionResponse_lastUpdatedAt,
    createCalculatedAttributeDefinitionResponse_statistic,
    createCalculatedAttributeDefinitionResponse_tags,
    createCalculatedAttributeDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCalculatedAttributeDefinition' smart constructor.
data CreateCalculatedAttributeDefinition = CreateCalculatedAttributeDefinition'
  { -- | The conditions including range, object count, and threshold for the
    -- calculated attribute.
    conditions :: Prelude.Maybe Conditions,
    -- | The description of the calculated attribute.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the calculated attribute.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Text,
    -- | Mathematical expression and a list of attribute items specified in that
    -- expression.
    attributeDetails :: AttributeDetails,
    -- | The aggregation operation to perform for the calculated attribute.
    statistic :: Statistic
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCalculatedAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditions', 'createCalculatedAttributeDefinition_conditions' - The conditions including range, object count, and threshold for the
-- calculated attribute.
--
-- 'description', 'createCalculatedAttributeDefinition_description' - The description of the calculated attribute.
--
-- 'displayName', 'createCalculatedAttributeDefinition_displayName' - The display name of the calculated attribute.
--
-- 'tags', 'createCalculatedAttributeDefinition_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'createCalculatedAttributeDefinition_domainName' - The unique name of the domain.
--
-- 'calculatedAttributeName', 'createCalculatedAttributeDefinition_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'attributeDetails', 'createCalculatedAttributeDefinition_attributeDetails' - Mathematical expression and a list of attribute items specified in that
-- expression.
--
-- 'statistic', 'createCalculatedAttributeDefinition_statistic' - The aggregation operation to perform for the calculated attribute.
newCreateCalculatedAttributeDefinition ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'calculatedAttributeName'
  Prelude.Text ->
  -- | 'attributeDetails'
  AttributeDetails ->
  -- | 'statistic'
  Statistic ->
  CreateCalculatedAttributeDefinition
newCreateCalculatedAttributeDefinition
  pDomainName_
  pCalculatedAttributeName_
  pAttributeDetails_
  pStatistic_ =
    CreateCalculatedAttributeDefinition'
      { conditions =
          Prelude.Nothing,
        description = Prelude.Nothing,
        displayName = Prelude.Nothing,
        tags = Prelude.Nothing,
        domainName = pDomainName_,
        calculatedAttributeName =
          pCalculatedAttributeName_,
        attributeDetails = pAttributeDetails_,
        statistic = pStatistic_
      }

-- | The conditions including range, object count, and threshold for the
-- calculated attribute.
createCalculatedAttributeDefinition_conditions :: Lens.Lens' CreateCalculatedAttributeDefinition (Prelude.Maybe Conditions)
createCalculatedAttributeDefinition_conditions = Lens.lens (\CreateCalculatedAttributeDefinition' {conditions} -> conditions) (\s@CreateCalculatedAttributeDefinition' {} a -> s {conditions = a} :: CreateCalculatedAttributeDefinition)

-- | The description of the calculated attribute.
createCalculatedAttributeDefinition_description :: Lens.Lens' CreateCalculatedAttributeDefinition (Prelude.Maybe Prelude.Text)
createCalculatedAttributeDefinition_description = Lens.lens (\CreateCalculatedAttributeDefinition' {description} -> description) (\s@CreateCalculatedAttributeDefinition' {} a -> s {description = a} :: CreateCalculatedAttributeDefinition)

-- | The display name of the calculated attribute.
createCalculatedAttributeDefinition_displayName :: Lens.Lens' CreateCalculatedAttributeDefinition (Prelude.Maybe Prelude.Text)
createCalculatedAttributeDefinition_displayName = Lens.lens (\CreateCalculatedAttributeDefinition' {displayName} -> displayName) (\s@CreateCalculatedAttributeDefinition' {} a -> s {displayName = a} :: CreateCalculatedAttributeDefinition)

-- | The tags used to organize, track, or control access for this resource.
createCalculatedAttributeDefinition_tags :: Lens.Lens' CreateCalculatedAttributeDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCalculatedAttributeDefinition_tags = Lens.lens (\CreateCalculatedAttributeDefinition' {tags} -> tags) (\s@CreateCalculatedAttributeDefinition' {} a -> s {tags = a} :: CreateCalculatedAttributeDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
createCalculatedAttributeDefinition_domainName :: Lens.Lens' CreateCalculatedAttributeDefinition Prelude.Text
createCalculatedAttributeDefinition_domainName = Lens.lens (\CreateCalculatedAttributeDefinition' {domainName} -> domainName) (\s@CreateCalculatedAttributeDefinition' {} a -> s {domainName = a} :: CreateCalculatedAttributeDefinition)

-- | The unique name of the calculated attribute.
createCalculatedAttributeDefinition_calculatedAttributeName :: Lens.Lens' CreateCalculatedAttributeDefinition Prelude.Text
createCalculatedAttributeDefinition_calculatedAttributeName = Lens.lens (\CreateCalculatedAttributeDefinition' {calculatedAttributeName} -> calculatedAttributeName) (\s@CreateCalculatedAttributeDefinition' {} a -> s {calculatedAttributeName = a} :: CreateCalculatedAttributeDefinition)

-- | Mathematical expression and a list of attribute items specified in that
-- expression.
createCalculatedAttributeDefinition_attributeDetails :: Lens.Lens' CreateCalculatedAttributeDefinition AttributeDetails
createCalculatedAttributeDefinition_attributeDetails = Lens.lens (\CreateCalculatedAttributeDefinition' {attributeDetails} -> attributeDetails) (\s@CreateCalculatedAttributeDefinition' {} a -> s {attributeDetails = a} :: CreateCalculatedAttributeDefinition)

-- | The aggregation operation to perform for the calculated attribute.
createCalculatedAttributeDefinition_statistic :: Lens.Lens' CreateCalculatedAttributeDefinition Statistic
createCalculatedAttributeDefinition_statistic = Lens.lens (\CreateCalculatedAttributeDefinition' {statistic} -> statistic) (\s@CreateCalculatedAttributeDefinition' {} a -> s {statistic = a} :: CreateCalculatedAttributeDefinition)

instance
  Core.AWSRequest
    CreateCalculatedAttributeDefinition
  where
  type
    AWSResponse CreateCalculatedAttributeDefinition =
      CreateCalculatedAttributeDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCalculatedAttributeDefinitionResponse'
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
    CreateCalculatedAttributeDefinition
  where
  hashWithSalt
    _salt
    CreateCalculatedAttributeDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` conditions
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` displayName
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` calculatedAttributeName
        `Prelude.hashWithSalt` attributeDetails
        `Prelude.hashWithSalt` statistic

instance
  Prelude.NFData
    CreateCalculatedAttributeDefinition
  where
  rnf CreateCalculatedAttributeDefinition' {..} =
    Prelude.rnf conditions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf calculatedAttributeName
      `Prelude.seq` Prelude.rnf attributeDetails
      `Prelude.seq` Prelude.rnf statistic

instance
  Data.ToHeaders
    CreateCalculatedAttributeDefinition
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
    CreateCalculatedAttributeDefinition
  where
  toJSON CreateCalculatedAttributeDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Conditions" Data..=) Prelude.<$> conditions,
            ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("AttributeDetails" Data..= attributeDetails),
            Prelude.Just ("Statistic" Data..= statistic)
          ]
      )

instance
  Data.ToPath
    CreateCalculatedAttributeDefinition
  where
  toPath CreateCalculatedAttributeDefinition' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/calculated-attributes/",
        Data.toBS calculatedAttributeName
      ]

instance
  Data.ToQuery
    CreateCalculatedAttributeDefinition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCalculatedAttributeDefinitionResponse' smart constructor.
data CreateCalculatedAttributeDefinitionResponse = CreateCalculatedAttributeDefinitionResponse'
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
-- Create a value of 'CreateCalculatedAttributeDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeDetails', 'createCalculatedAttributeDefinitionResponse_attributeDetails' - Mathematical expression and a list of attribute items specified in that
-- expression.
--
-- 'calculatedAttributeName', 'createCalculatedAttributeDefinitionResponse_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'conditions', 'createCalculatedAttributeDefinitionResponse_conditions' - The conditions including range, object count, and threshold for the
-- calculated attribute.
--
-- 'createdAt', 'createCalculatedAttributeDefinitionResponse_createdAt' - The timestamp of when the calculated attribute definition was created.
--
-- 'description', 'createCalculatedAttributeDefinitionResponse_description' - The description of the calculated attribute.
--
-- 'displayName', 'createCalculatedAttributeDefinitionResponse_displayName' - The display name of the calculated attribute.
--
-- 'lastUpdatedAt', 'createCalculatedAttributeDefinitionResponse_lastUpdatedAt' - The timestamp of when the calculated attribute definition was most
-- recently edited.
--
-- 'statistic', 'createCalculatedAttributeDefinitionResponse_statistic' - The aggregation operation to perform for the calculated attribute.
--
-- 'tags', 'createCalculatedAttributeDefinitionResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'createCalculatedAttributeDefinitionResponse_httpStatus' - The response's http status code.
newCreateCalculatedAttributeDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCalculatedAttributeDefinitionResponse
newCreateCalculatedAttributeDefinitionResponse
  pHttpStatus_ =
    CreateCalculatedAttributeDefinitionResponse'
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

-- | Mathematical expression and a list of attribute items specified in that
-- expression.
createCalculatedAttributeDefinitionResponse_attributeDetails :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe AttributeDetails)
createCalculatedAttributeDefinitionResponse_attributeDetails = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {attributeDetails} -> attributeDetails) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {attributeDetails = a} :: CreateCalculatedAttributeDefinitionResponse)

-- | The unique name of the calculated attribute.
createCalculatedAttributeDefinitionResponse_calculatedAttributeName :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
createCalculatedAttributeDefinitionResponse_calculatedAttributeName = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {calculatedAttributeName} -> calculatedAttributeName) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {calculatedAttributeName = a} :: CreateCalculatedAttributeDefinitionResponse)

-- | The conditions including range, object count, and threshold for the
-- calculated attribute.
createCalculatedAttributeDefinitionResponse_conditions :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Conditions)
createCalculatedAttributeDefinitionResponse_conditions = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {conditions} -> conditions) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {conditions = a} :: CreateCalculatedAttributeDefinitionResponse)

-- | The timestamp of when the calculated attribute definition was created.
createCalculatedAttributeDefinitionResponse_createdAt :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
createCalculatedAttributeDefinitionResponse_createdAt = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {createdAt} -> createdAt) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {createdAt = a} :: CreateCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the calculated attribute.
createCalculatedAttributeDefinitionResponse_description :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
createCalculatedAttributeDefinitionResponse_description = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {description} -> description) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {description = a} :: CreateCalculatedAttributeDefinitionResponse)

-- | The display name of the calculated attribute.
createCalculatedAttributeDefinitionResponse_displayName :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.Text)
createCalculatedAttributeDefinitionResponse_displayName = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {displayName} -> displayName) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {displayName = a} :: CreateCalculatedAttributeDefinitionResponse)

-- | The timestamp of when the calculated attribute definition was most
-- recently edited.
createCalculatedAttributeDefinitionResponse_lastUpdatedAt :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Prelude.UTCTime)
createCalculatedAttributeDefinitionResponse_lastUpdatedAt = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {lastUpdatedAt = a} :: CreateCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Data._Time

-- | The aggregation operation to perform for the calculated attribute.
createCalculatedAttributeDefinitionResponse_statistic :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe Statistic)
createCalculatedAttributeDefinitionResponse_statistic = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {statistic} -> statistic) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {statistic = a} :: CreateCalculatedAttributeDefinitionResponse)

-- | The tags used to organize, track, or control access for this resource.
createCalculatedAttributeDefinitionResponse_tags :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createCalculatedAttributeDefinitionResponse_tags = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {tags} -> tags) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {tags = a} :: CreateCalculatedAttributeDefinitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createCalculatedAttributeDefinitionResponse_httpStatus :: Lens.Lens' CreateCalculatedAttributeDefinitionResponse Prelude.Int
createCalculatedAttributeDefinitionResponse_httpStatus = Lens.lens (\CreateCalculatedAttributeDefinitionResponse' {httpStatus} -> httpStatus) (\s@CreateCalculatedAttributeDefinitionResponse' {} a -> s {httpStatus = a} :: CreateCalculatedAttributeDefinitionResponse)

instance
  Prelude.NFData
    CreateCalculatedAttributeDefinitionResponse
  where
  rnf CreateCalculatedAttributeDefinitionResponse' {..} =
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
