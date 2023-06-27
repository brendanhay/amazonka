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
-- Module      : Amazonka.ResilienceHub.PutDraftAppVersionTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the app template for an Resilience Hub application draft
-- version.
module Amazonka.ResilienceHub.PutDraftAppVersionTemplate
  ( -- * Creating a Request
    PutDraftAppVersionTemplate (..),
    newPutDraftAppVersionTemplate,

    -- * Request Lenses
    putDraftAppVersionTemplate_appArn,
    putDraftAppVersionTemplate_appTemplateBody,

    -- * Destructuring the Response
    PutDraftAppVersionTemplateResponse (..),
    newPutDraftAppVersionTemplateResponse,

    -- * Response Lenses
    putDraftAppVersionTemplateResponse_appArn,
    putDraftAppVersionTemplateResponse_appVersion,
    putDraftAppVersionTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDraftAppVersionTemplate' smart constructor.
data PutDraftAppVersionTemplate = PutDraftAppVersionTemplate'
  { -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Text,
    -- | A JSON string that provides information about your application
    -- structure. To learn more about the @appTemplateBody@ template, see the
    -- sample template provided in the /Examples/ section.
    --
    -- The @appTemplateBody@ JSON string has the following structure:
    --
    -- -   __@resources@__
    --
    --     The list of logical resources that must be included in the
    --     Resilience Hub application.
    --
    --     Type: Array
    --
    --     Don\'t add the resources that you want to exclude.
    --
    --     Each @resources@ array item includes the following fields:
    --
    --     -   /@logicalResourceId@/
    --
    --         The logical identifier of the resource.
    --
    --         Type: Object
    --
    --         Each @logicalResourceId@ object includes the following fields:
    --
    --         -   @identifier@
    --
    --             The identifier of the resource.
    --
    --             Type: String
    --
    --         -   @logicalStackName@
    --
    --             The name of the CloudFormation stack this resource belongs
    --             to.
    --
    --             Type: String
    --
    --         -   @resourceGroupName@
    --
    --             The name of the resource group this resource belongs to.
    --
    --             Type: String
    --
    --         -   @terraformSourceName@
    --
    --             The name of the Terraform S3 state file this resource
    --             belongs to.
    --
    --             Type: String
    --
    --         -   @eksSourceName@
    --
    --             The name of the Amazon Elastic Kubernetes Service cluster
    --             and namespace this resource belongs to.
    --
    --             This parameter accepts values in \"eks-cluster\/namespace\"
    --             format.
    --
    --             Type: String
    --
    --     -   /@type@/
    --
    --         The type of resource.
    --
    --         Type: string
    --
    --     -   /@name@/
    --
    --         The name of the resource.
    --
    --         Type: String
    --
    --     -   @additionalInfo@
    --
    --         Additional configuration parameters for an Resilience Hub
    --         application. If you want to implement @additionalInfo@ through
    --         the Resilience Hub console rather than using an API call, see
    --         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    --         Currently, this parameter accepts a key-value mapping (in a
    --         string format) of only one failover region and one associated
    --         account.
    --
    --         Key: @\"failover-regions\"@
    --
    --         Value:
    --         @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
    --
    -- -   __@appComponents@__
    --
    --     The list of Application Components that this resource belongs to. If
    --     an Application Component is not part of the Resilience Hub
    --     application, it will be added.
    --
    --     Type: Array
    --
    --     Each @appComponents@ array item includes the following fields:
    --
    --     -   @name@
    --
    --         The name of the Application Component.
    --
    --         Type: String
    --
    --     -   @type@
    --
    --         The type of Application Component. For more information about
    --         the types of Application Component, see
    --         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
    --
    --         Type: String
    --
    --     -   @resourceNames@
    --
    --         The list of included resources that are assigned to the
    --         Application Component.
    --
    --         Type: Array of strings
    --
    --     -   @additionalInfo@
    --
    --         Additional configuration parameters for an Resilience Hub
    --         application. If you want to implement @additionalInfo@ through
    --         the Resilience Hub console rather than using an API call, see
    --         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    --         Currently, this parameter accepts a key-value mapping (in a
    --         string format) of only one failover region and one associated
    --         account.
    --
    --         Key: @\"failover-regions\"@
    --
    --         Value:
    --         @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
    --
    -- -   __@excludedResources@__
    --
    --     The list of logical resource identifiers to be excluded from the
    --     application.
    --
    --     Type: Array
    --
    --     Don\'t add the resources that you want to include.
    --
    --     Each @excludedResources@ array item includes the following fields:
    --
    --     -   /@logicalResourceIds@/
    --
    --         The logical identifier of the resource.
    --
    --         Type: Object
    --
    --         You can configure only one of the following fields:
    --
    --         -   @logicalStackName@
    --
    --         -   @resourceGroupName@
    --
    --         -   @terraformSourceName@
    --
    --         -   @eksSourceName@
    --
    --         Each @logicalResourceIds@ object includes the following fields:
    --
    --         -   @identifier@
    --
    --             The identifier of the resource.
    --
    --             Type: String
    --
    --         -   @logicalStackName@
    --
    --             The name of the CloudFormation stack this resource belongs
    --             to.
    --
    --             Type: String
    --
    --         -   @resourceGroupName@
    --
    --             The name of the resource group this resource belongs to.
    --
    --             Type: String
    --
    --         -   @terraformSourceName@
    --
    --             The name of the Terraform S3 state file this resource
    --             belongs to.
    --
    --             Type: String
    --
    --         -   @eksSourceName@
    --
    --             The name of the Amazon Elastic Kubernetes Service cluster
    --             and namespace this resource belongs to.
    --
    --             This parameter accepts values in \"eks-cluster\/namespace\"
    --             format.
    --
    --             Type: String
    --
    -- -   __@version@__
    --
    --     The Resilience Hub application version.
    --
    -- -   @additionalInfo@
    --
    --     Additional configuration parameters for an Resilience Hub
    --     application. If you want to implement @additionalInfo@ through the
    --     Resilience Hub console rather than using an API call, see
    --     <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    --     Currently, this parameter accepts a key-value mapping (in a string
    --     format) of only one failover region and one associated account.
    --
    --     Key: @\"failover-regions\"@
    --
    --     Value:
    --     @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
    appTemplateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDraftAppVersionTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'putDraftAppVersionTemplate_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appTemplateBody', 'putDraftAppVersionTemplate_appTemplateBody' - A JSON string that provides information about your application
-- structure. To learn more about the @appTemplateBody@ template, see the
-- sample template provided in the /Examples/ section.
--
-- The @appTemplateBody@ JSON string has the following structure:
--
-- -   __@resources@__
--
--     The list of logical resources that must be included in the
--     Resilience Hub application.
--
--     Type: Array
--
--     Don\'t add the resources that you want to exclude.
--
--     Each @resources@ array item includes the following fields:
--
--     -   /@logicalResourceId@/
--
--         The logical identifier of the resource.
--
--         Type: Object
--
--         Each @logicalResourceId@ object includes the following fields:
--
--         -   @identifier@
--
--             The identifier of the resource.
--
--             Type: String
--
--         -   @logicalStackName@
--
--             The name of the CloudFormation stack this resource belongs
--             to.
--
--             Type: String
--
--         -   @resourceGroupName@
--
--             The name of the resource group this resource belongs to.
--
--             Type: String
--
--         -   @terraformSourceName@
--
--             The name of the Terraform S3 state file this resource
--             belongs to.
--
--             Type: String
--
--         -   @eksSourceName@
--
--             The name of the Amazon Elastic Kubernetes Service cluster
--             and namespace this resource belongs to.
--
--             This parameter accepts values in \"eks-cluster\/namespace\"
--             format.
--
--             Type: String
--
--     -   /@type@/
--
--         The type of resource.
--
--         Type: string
--
--     -   /@name@/
--
--         The name of the resource.
--
--         Type: String
--
--     -   @additionalInfo@
--
--         Additional configuration parameters for an Resilience Hub
--         application. If you want to implement @additionalInfo@ through
--         the Resilience Hub console rather than using an API call, see
--         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
--         Currently, this parameter accepts a key-value mapping (in a
--         string format) of only one failover region and one associated
--         account.
--
--         Key: @\"failover-regions\"@
--
--         Value:
--         @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- -   __@appComponents@__
--
--     The list of Application Components that this resource belongs to. If
--     an Application Component is not part of the Resilience Hub
--     application, it will be added.
--
--     Type: Array
--
--     Each @appComponents@ array item includes the following fields:
--
--     -   @name@
--
--         The name of the Application Component.
--
--         Type: String
--
--     -   @type@
--
--         The type of Application Component. For more information about
--         the types of Application Component, see
--         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
--
--         Type: String
--
--     -   @resourceNames@
--
--         The list of included resources that are assigned to the
--         Application Component.
--
--         Type: Array of strings
--
--     -   @additionalInfo@
--
--         Additional configuration parameters for an Resilience Hub
--         application. If you want to implement @additionalInfo@ through
--         the Resilience Hub console rather than using an API call, see
--         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
--         Currently, this parameter accepts a key-value mapping (in a
--         string format) of only one failover region and one associated
--         account.
--
--         Key: @\"failover-regions\"@
--
--         Value:
--         @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- -   __@excludedResources@__
--
--     The list of logical resource identifiers to be excluded from the
--     application.
--
--     Type: Array
--
--     Don\'t add the resources that you want to include.
--
--     Each @excludedResources@ array item includes the following fields:
--
--     -   /@logicalResourceIds@/
--
--         The logical identifier of the resource.
--
--         Type: Object
--
--         You can configure only one of the following fields:
--
--         -   @logicalStackName@
--
--         -   @resourceGroupName@
--
--         -   @terraformSourceName@
--
--         -   @eksSourceName@
--
--         Each @logicalResourceIds@ object includes the following fields:
--
--         -   @identifier@
--
--             The identifier of the resource.
--
--             Type: String
--
--         -   @logicalStackName@
--
--             The name of the CloudFormation stack this resource belongs
--             to.
--
--             Type: String
--
--         -   @resourceGroupName@
--
--             The name of the resource group this resource belongs to.
--
--             Type: String
--
--         -   @terraformSourceName@
--
--             The name of the Terraform S3 state file this resource
--             belongs to.
--
--             Type: String
--
--         -   @eksSourceName@
--
--             The name of the Amazon Elastic Kubernetes Service cluster
--             and namespace this resource belongs to.
--
--             This parameter accepts values in \"eks-cluster\/namespace\"
--             format.
--
--             Type: String
--
-- -   __@version@__
--
--     The Resilience Hub application version.
--
-- -   @additionalInfo@
--
--     Additional configuration parameters for an Resilience Hub
--     application. If you want to implement @additionalInfo@ through the
--     Resilience Hub console rather than using an API call, see
--     <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
--     Currently, this parameter accepts a key-value mapping (in a string
--     format) of only one failover region and one associated account.
--
--     Key: @\"failover-regions\"@
--
--     Value:
--     @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
newPutDraftAppVersionTemplate ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appTemplateBody'
  Prelude.Text ->
  PutDraftAppVersionTemplate
newPutDraftAppVersionTemplate
  pAppArn_
  pAppTemplateBody_ =
    PutDraftAppVersionTemplate'
      { appArn = pAppArn_,
        appTemplateBody = pAppTemplateBody_
      }

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
putDraftAppVersionTemplate_appArn :: Lens.Lens' PutDraftAppVersionTemplate Prelude.Text
putDraftAppVersionTemplate_appArn = Lens.lens (\PutDraftAppVersionTemplate' {appArn} -> appArn) (\s@PutDraftAppVersionTemplate' {} a -> s {appArn = a} :: PutDraftAppVersionTemplate)

-- | A JSON string that provides information about your application
-- structure. To learn more about the @appTemplateBody@ template, see the
-- sample template provided in the /Examples/ section.
--
-- The @appTemplateBody@ JSON string has the following structure:
--
-- -   __@resources@__
--
--     The list of logical resources that must be included in the
--     Resilience Hub application.
--
--     Type: Array
--
--     Don\'t add the resources that you want to exclude.
--
--     Each @resources@ array item includes the following fields:
--
--     -   /@logicalResourceId@/
--
--         The logical identifier of the resource.
--
--         Type: Object
--
--         Each @logicalResourceId@ object includes the following fields:
--
--         -   @identifier@
--
--             The identifier of the resource.
--
--             Type: String
--
--         -   @logicalStackName@
--
--             The name of the CloudFormation stack this resource belongs
--             to.
--
--             Type: String
--
--         -   @resourceGroupName@
--
--             The name of the resource group this resource belongs to.
--
--             Type: String
--
--         -   @terraformSourceName@
--
--             The name of the Terraform S3 state file this resource
--             belongs to.
--
--             Type: String
--
--         -   @eksSourceName@
--
--             The name of the Amazon Elastic Kubernetes Service cluster
--             and namespace this resource belongs to.
--
--             This parameter accepts values in \"eks-cluster\/namespace\"
--             format.
--
--             Type: String
--
--     -   /@type@/
--
--         The type of resource.
--
--         Type: string
--
--     -   /@name@/
--
--         The name of the resource.
--
--         Type: String
--
--     -   @additionalInfo@
--
--         Additional configuration parameters for an Resilience Hub
--         application. If you want to implement @additionalInfo@ through
--         the Resilience Hub console rather than using an API call, see
--         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
--         Currently, this parameter accepts a key-value mapping (in a
--         string format) of only one failover region and one associated
--         account.
--
--         Key: @\"failover-regions\"@
--
--         Value:
--         @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- -   __@appComponents@__
--
--     The list of Application Components that this resource belongs to. If
--     an Application Component is not part of the Resilience Hub
--     application, it will be added.
--
--     Type: Array
--
--     Each @appComponents@ array item includes the following fields:
--
--     -   @name@
--
--         The name of the Application Component.
--
--         Type: String
--
--     -   @type@
--
--         The type of Application Component. For more information about
--         the types of Application Component, see
--         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/AppComponent.grouping.html Grouping resources in an AppComponent>.
--
--         Type: String
--
--     -   @resourceNames@
--
--         The list of included resources that are assigned to the
--         Application Component.
--
--         Type: Array of strings
--
--     -   @additionalInfo@
--
--         Additional configuration parameters for an Resilience Hub
--         application. If you want to implement @additionalInfo@ through
--         the Resilience Hub console rather than using an API call, see
--         <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
--         Currently, this parameter accepts a key-value mapping (in a
--         string format) of only one failover region and one associated
--         account.
--
--         Key: @\"failover-regions\"@
--
--         Value:
--         @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- -   __@excludedResources@__
--
--     The list of logical resource identifiers to be excluded from the
--     application.
--
--     Type: Array
--
--     Don\'t add the resources that you want to include.
--
--     Each @excludedResources@ array item includes the following fields:
--
--     -   /@logicalResourceIds@/
--
--         The logical identifier of the resource.
--
--         Type: Object
--
--         You can configure only one of the following fields:
--
--         -   @logicalStackName@
--
--         -   @resourceGroupName@
--
--         -   @terraformSourceName@
--
--         -   @eksSourceName@
--
--         Each @logicalResourceIds@ object includes the following fields:
--
--         -   @identifier@
--
--             The identifier of the resource.
--
--             Type: String
--
--         -   @logicalStackName@
--
--             The name of the CloudFormation stack this resource belongs
--             to.
--
--             Type: String
--
--         -   @resourceGroupName@
--
--             The name of the resource group this resource belongs to.
--
--             Type: String
--
--         -   @terraformSourceName@
--
--             The name of the Terraform S3 state file this resource
--             belongs to.
--
--             Type: String
--
--         -   @eksSourceName@
--
--             The name of the Amazon Elastic Kubernetes Service cluster
--             and namespace this resource belongs to.
--
--             This parameter accepts values in \"eks-cluster\/namespace\"
--             format.
--
--             Type: String
--
-- -   __@version@__
--
--     The Resilience Hub application version.
--
-- -   @additionalInfo@
--
--     Additional configuration parameters for an Resilience Hub
--     application. If you want to implement @additionalInfo@ through the
--     Resilience Hub console rather than using an API call, see
--     <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
--     Currently, this parameter accepts a key-value mapping (in a string
--     format) of only one failover region and one associated account.
--
--     Key: @\"failover-regions\"@
--
--     Value:
--     @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
putDraftAppVersionTemplate_appTemplateBody :: Lens.Lens' PutDraftAppVersionTemplate Prelude.Text
putDraftAppVersionTemplate_appTemplateBody = Lens.lens (\PutDraftAppVersionTemplate' {appTemplateBody} -> appTemplateBody) (\s@PutDraftAppVersionTemplate' {} a -> s {appTemplateBody = a} :: PutDraftAppVersionTemplate)

instance Core.AWSRequest PutDraftAppVersionTemplate where
  type
    AWSResponse PutDraftAppVersionTemplate =
      PutDraftAppVersionTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDraftAppVersionTemplateResponse'
            Prelude.<$> (x Data..?> "appArn")
            Prelude.<*> (x Data..?> "appVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDraftAppVersionTemplate where
  hashWithSalt _salt PutDraftAppVersionTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` appArn
      `Prelude.hashWithSalt` appTemplateBody

instance Prelude.NFData PutDraftAppVersionTemplate where
  rnf PutDraftAppVersionTemplate' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appTemplateBody

instance Data.ToHeaders PutDraftAppVersionTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDraftAppVersionTemplate where
  toJSON PutDraftAppVersionTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just
              ("appTemplateBody" Data..= appTemplateBody)
          ]
      )

instance Data.ToPath PutDraftAppVersionTemplate where
  toPath =
    Prelude.const "/put-draft-app-version-template"

instance Data.ToQuery PutDraftAppVersionTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDraftAppVersionTemplateResponse' smart constructor.
data PutDraftAppVersionTemplateResponse = PutDraftAppVersionTemplateResponse'
  { -- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
    -- format for this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
    -- information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    appArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDraftAppVersionTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appArn', 'putDraftAppVersionTemplateResponse_appArn' - The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
--
-- 'appVersion', 'putDraftAppVersionTemplateResponse_appVersion' - The version of the application.
--
-- 'httpStatus', 'putDraftAppVersionTemplateResponse_httpStatus' - The response's http status code.
newPutDraftAppVersionTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDraftAppVersionTemplateResponse
newPutDraftAppVersionTemplateResponse pHttpStatus_ =
  PutDraftAppVersionTemplateResponse'
    { appArn =
        Prelude.Nothing,
      appVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Resilience Hub application. The
-- format for this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@. For more
-- information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
putDraftAppVersionTemplateResponse_appArn :: Lens.Lens' PutDraftAppVersionTemplateResponse (Prelude.Maybe Prelude.Text)
putDraftAppVersionTemplateResponse_appArn = Lens.lens (\PutDraftAppVersionTemplateResponse' {appArn} -> appArn) (\s@PutDraftAppVersionTemplateResponse' {} a -> s {appArn = a} :: PutDraftAppVersionTemplateResponse)

-- | The version of the application.
putDraftAppVersionTemplateResponse_appVersion :: Lens.Lens' PutDraftAppVersionTemplateResponse (Prelude.Maybe Prelude.Text)
putDraftAppVersionTemplateResponse_appVersion = Lens.lens (\PutDraftAppVersionTemplateResponse' {appVersion} -> appVersion) (\s@PutDraftAppVersionTemplateResponse' {} a -> s {appVersion = a} :: PutDraftAppVersionTemplateResponse)

-- | The response's http status code.
putDraftAppVersionTemplateResponse_httpStatus :: Lens.Lens' PutDraftAppVersionTemplateResponse Prelude.Int
putDraftAppVersionTemplateResponse_httpStatus = Lens.lens (\PutDraftAppVersionTemplateResponse' {httpStatus} -> httpStatus) (\s@PutDraftAppVersionTemplateResponse' {} a -> s {httpStatus = a} :: PutDraftAppVersionTemplateResponse)

instance
  Prelude.NFData
    PutDraftAppVersionTemplateResponse
  where
  rnf PutDraftAppVersionTemplateResponse' {..} =
    Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion
      `Prelude.seq` Prelude.rnf httpStatus
