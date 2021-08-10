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
-- Module      : Network.AWS.SSM.CreateActivation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates an activation code and activation ID you can use to register
-- your on-premises server or virtual machine (VM) with Systems Manager.
-- Registering these machines with Systems Manager makes it possible to
-- manage them using Systems Manager capabilities. You use the activation
-- code and ID when installing SSM Agent on machines in your hybrid
-- environment. For more information about requirements for managing
-- on-premises instances and VMs using Systems Manager, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances.html Setting up AWS Systems Manager for hybrid environments>
-- in the /AWS Systems Manager User Guide/.
--
-- On-premises servers or VMs that are registered with Systems Manager and
-- EC2 instances that you manage with Systems Manager are all called
-- /managed instances/.
module Network.AWS.SSM.CreateActivation
  ( -- * Creating a Request
    CreateActivation (..),
    newCreateActivation,

    -- * Request Lenses
    createActivation_registrationLimit,
    createActivation_defaultInstanceName,
    createActivation_expirationDate,
    createActivation_tags,
    createActivation_description,
    createActivation_iamRole,

    -- * Destructuring the Response
    CreateActivationResponse (..),
    newCreateActivationResponse,

    -- * Response Lenses
    createActivationResponse_activationCode,
    createActivationResponse_activationId,
    createActivationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateActivation' smart constructor.
data CreateActivation = CreateActivation'
  { -- | Specify the maximum number of managed instances you want to register.
    -- The default value is 1 instance.
    registrationLimit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the registered, managed instance as it will appear in the
    -- Systems Manager console or when you use the AWS command line tools to
    -- list Systems Manager resources.
    --
    -- Do not enter personally identifiable information in this field.
    defaultInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The date by which this activation request should expire. The default
    -- value is 24 hours.
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag an activation to
    -- identify which servers or virtual machines (VMs) in your on-premises
    -- environment you intend to activate. In this case, you could specify the
    -- following key name\/value pairs:
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- When you install SSM Agent on your on-premises servers and VMs, you
    -- specify an activation ID and code. When you specify the activation ID
    -- and code, tags assigned to the activation are automatically applied to
    -- the on-premises servers or VMs.
    --
    -- You can\'t add tags to or delete tags from an existing activation. You
    -- can tag your on-premises servers and VMs after they connect to Systems
    -- Manager for the first time and are assigned a managed instance ID. This
    -- means they are listed in the AWS Systems Manager console with an ID that
    -- is prefixed with \"mi-\". For information about how to add tags to your
    -- managed instances, see AddTagsToResource. For information about how to
    -- remove tags from your managed instances, see RemoveTagsFromResource.
    tags :: Prelude.Maybe [Tag],
    -- | A user-defined description of the resource that you want to register
    -- with Systems Manager.
    --
    -- Do not enter personally identifiable information in this field.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Identity and Access Management (IAM) role that you want to
    -- assign to the managed instance. This IAM role must provide AssumeRole
    -- permissions for the Systems Manager service principal
    -- @ssm.amazonaws.com@. For more information, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment>
    -- in the /AWS Systems Manager User Guide/.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateActivation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationLimit', 'createActivation_registrationLimit' - Specify the maximum number of managed instances you want to register.
-- The default value is 1 instance.
--
-- 'defaultInstanceName', 'createActivation_defaultInstanceName' - The name of the registered, managed instance as it will appear in the
-- Systems Manager console or when you use the AWS command line tools to
-- list Systems Manager resources.
--
-- Do not enter personally identifiable information in this field.
--
-- 'expirationDate', 'createActivation_expirationDate' - The date by which this activation request should expire. The default
-- value is 24 hours.
--
-- 'tags', 'createActivation_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an activation to
-- identify which servers or virtual machines (VMs) in your on-premises
-- environment you intend to activate. In this case, you could specify the
-- following key name\/value pairs:
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- When you install SSM Agent on your on-premises servers and VMs, you
-- specify an activation ID and code. When you specify the activation ID
-- and code, tags assigned to the activation are automatically applied to
-- the on-premises servers or VMs.
--
-- You can\'t add tags to or delete tags from an existing activation. You
-- can tag your on-premises servers and VMs after they connect to Systems
-- Manager for the first time and are assigned a managed instance ID. This
-- means they are listed in the AWS Systems Manager console with an ID that
-- is prefixed with \"mi-\". For information about how to add tags to your
-- managed instances, see AddTagsToResource. For information about how to
-- remove tags from your managed instances, see RemoveTagsFromResource.
--
-- 'description', 'createActivation_description' - A user-defined description of the resource that you want to register
-- with Systems Manager.
--
-- Do not enter personally identifiable information in this field.
--
-- 'iamRole', 'createActivation_iamRole' - The Amazon Identity and Access Management (IAM) role that you want to
-- assign to the managed instance. This IAM role must provide AssumeRole
-- permissions for the Systems Manager service principal
-- @ssm.amazonaws.com@. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment>
-- in the /AWS Systems Manager User Guide/.
newCreateActivation ::
  -- | 'iamRole'
  Prelude.Text ->
  CreateActivation
newCreateActivation pIamRole_ =
  CreateActivation'
    { registrationLimit =
        Prelude.Nothing,
      defaultInstanceName = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      iamRole = pIamRole_
    }

-- | Specify the maximum number of managed instances you want to register.
-- The default value is 1 instance.
createActivation_registrationLimit :: Lens.Lens' CreateActivation (Prelude.Maybe Prelude.Natural)
createActivation_registrationLimit = Lens.lens (\CreateActivation' {registrationLimit} -> registrationLimit) (\s@CreateActivation' {} a -> s {registrationLimit = a} :: CreateActivation)

-- | The name of the registered, managed instance as it will appear in the
-- Systems Manager console or when you use the AWS command line tools to
-- list Systems Manager resources.
--
-- Do not enter personally identifiable information in this field.
createActivation_defaultInstanceName :: Lens.Lens' CreateActivation (Prelude.Maybe Prelude.Text)
createActivation_defaultInstanceName = Lens.lens (\CreateActivation' {defaultInstanceName} -> defaultInstanceName) (\s@CreateActivation' {} a -> s {defaultInstanceName = a} :: CreateActivation)

-- | The date by which this activation request should expire. The default
-- value is 24 hours.
createActivation_expirationDate :: Lens.Lens' CreateActivation (Prelude.Maybe Prelude.UTCTime)
createActivation_expirationDate = Lens.lens (\CreateActivation' {expirationDate} -> expirationDate) (\s@CreateActivation' {} a -> s {expirationDate = a} :: CreateActivation) Prelude.. Lens.mapping Core._Time

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag an activation to
-- identify which servers or virtual machines (VMs) in your on-premises
-- environment you intend to activate. In this case, you could specify the
-- following key name\/value pairs:
--
-- -   @Key=OS,Value=Windows@
--
-- -   @Key=Environment,Value=Production@
--
-- When you install SSM Agent on your on-premises servers and VMs, you
-- specify an activation ID and code. When you specify the activation ID
-- and code, tags assigned to the activation are automatically applied to
-- the on-premises servers or VMs.
--
-- You can\'t add tags to or delete tags from an existing activation. You
-- can tag your on-premises servers and VMs after they connect to Systems
-- Manager for the first time and are assigned a managed instance ID. This
-- means they are listed in the AWS Systems Manager console with an ID that
-- is prefixed with \"mi-\". For information about how to add tags to your
-- managed instances, see AddTagsToResource. For information about how to
-- remove tags from your managed instances, see RemoveTagsFromResource.
createActivation_tags :: Lens.Lens' CreateActivation (Prelude.Maybe [Tag])
createActivation_tags = Lens.lens (\CreateActivation' {tags} -> tags) (\s@CreateActivation' {} a -> s {tags = a} :: CreateActivation) Prelude.. Lens.mapping Lens._Coerce

-- | A user-defined description of the resource that you want to register
-- with Systems Manager.
--
-- Do not enter personally identifiable information in this field.
createActivation_description :: Lens.Lens' CreateActivation (Prelude.Maybe Prelude.Text)
createActivation_description = Lens.lens (\CreateActivation' {description} -> description) (\s@CreateActivation' {} a -> s {description = a} :: CreateActivation)

-- | The Amazon Identity and Access Management (IAM) role that you want to
-- assign to the managed instance. This IAM role must provide AssumeRole
-- permissions for the Systems Manager service principal
-- @ssm.amazonaws.com@. For more information, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-service-role.html Create an IAM service role for a hybrid environment>
-- in the /AWS Systems Manager User Guide/.
createActivation_iamRole :: Lens.Lens' CreateActivation Prelude.Text
createActivation_iamRole = Lens.lens (\CreateActivation' {iamRole} -> iamRole) (\s@CreateActivation' {} a -> s {iamRole = a} :: CreateActivation)

instance Core.AWSRequest CreateActivation where
  type
    AWSResponse CreateActivation =
      CreateActivationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateActivationResponse'
            Prelude.<$> (x Core..?> "ActivationCode")
            Prelude.<*> (x Core..?> "ActivationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateActivation

instance Prelude.NFData CreateActivation

instance Core.ToHeaders CreateActivation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.CreateActivation" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateActivation where
  toJSON CreateActivation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RegistrationLimit" Core..=)
              Prelude.<$> registrationLimit,
            ("DefaultInstanceName" Core..=)
              Prelude.<$> defaultInstanceName,
            ("ExpirationDate" Core..=)
              Prelude.<$> expirationDate,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("IamRole" Core..= iamRole)
          ]
      )

instance Core.ToPath CreateActivation where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateActivation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateActivationResponse' smart constructor.
data CreateActivationResponse = CreateActivationResponse'
  { -- | The code the system generates when it processes the activation. The
    -- activation code functions like a password to validate the activation ID.
    activationCode :: Prelude.Maybe Prelude.Text,
    -- | The ID number generated by the system when it processed the activation.
    -- The activation ID functions like a user name.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateActivationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationCode', 'createActivationResponse_activationCode' - The code the system generates when it processes the activation. The
-- activation code functions like a password to validate the activation ID.
--
-- 'activationId', 'createActivationResponse_activationId' - The ID number generated by the system when it processed the activation.
-- The activation ID functions like a user name.
--
-- 'httpStatus', 'createActivationResponse_httpStatus' - The response's http status code.
newCreateActivationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateActivationResponse
newCreateActivationResponse pHttpStatus_ =
  CreateActivationResponse'
    { activationCode =
        Prelude.Nothing,
      activationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The code the system generates when it processes the activation. The
-- activation code functions like a password to validate the activation ID.
createActivationResponse_activationCode :: Lens.Lens' CreateActivationResponse (Prelude.Maybe Prelude.Text)
createActivationResponse_activationCode = Lens.lens (\CreateActivationResponse' {activationCode} -> activationCode) (\s@CreateActivationResponse' {} a -> s {activationCode = a} :: CreateActivationResponse)

-- | The ID number generated by the system when it processed the activation.
-- The activation ID functions like a user name.
createActivationResponse_activationId :: Lens.Lens' CreateActivationResponse (Prelude.Maybe Prelude.Text)
createActivationResponse_activationId = Lens.lens (\CreateActivationResponse' {activationId} -> activationId) (\s@CreateActivationResponse' {} a -> s {activationId = a} :: CreateActivationResponse)

-- | The response's http status code.
createActivationResponse_httpStatus :: Lens.Lens' CreateActivationResponse Prelude.Int
createActivationResponse_httpStatus = Lens.lens (\CreateActivationResponse' {httpStatus} -> httpStatus) (\s@CreateActivationResponse' {} a -> s {httpStatus = a} :: CreateActivationResponse)

instance Prelude.NFData CreateActivationResponse
