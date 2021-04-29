{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectoryService.CreateComputer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Active Directory computer object in the specified directory.
module Network.AWS.DirectoryService.CreateComputer
  ( -- * Creating a Request
    CreateComputer (..),
    newCreateComputer,

    -- * Request Lenses
    createComputer_organizationalUnitDistinguishedName,
    createComputer_computerAttributes,
    createComputer_directoryId,
    createComputer_computerName,
    createComputer_password,

    -- * Destructuring the Response
    CreateComputerResponse (..),
    newCreateComputerResponse,

    -- * Response Lenses
    createComputerResponse_computer,
    createComputerResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the CreateComputer operation.
--
-- /See:/ 'newCreateComputer' smart constructor.
data CreateComputer = CreateComputer'
  { -- | The fully-qualified distinguished name of the organizational unit to
    -- place the computer account in.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text,
    -- | An array of Attribute objects that contain any LDAP attributes to apply
    -- to the computer account.
    computerAttributes :: Prelude.Maybe [Attribute],
    -- | The identifier of the directory in which to create the computer account.
    directoryId :: Prelude.Text,
    -- | The name of the computer account.
    computerName :: Prelude.Text,
    -- | A one-time password that is used to join the computer to the directory.
    -- You should generate a random, strong password to use for this parameter.
    password :: Prelude.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateComputer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationalUnitDistinguishedName', 'createComputer_organizationalUnitDistinguishedName' - The fully-qualified distinguished name of the organizational unit to
-- place the computer account in.
--
-- 'computerAttributes', 'createComputer_computerAttributes' - An array of Attribute objects that contain any LDAP attributes to apply
-- to the computer account.
--
-- 'directoryId', 'createComputer_directoryId' - The identifier of the directory in which to create the computer account.
--
-- 'computerName', 'createComputer_computerName' - The name of the computer account.
--
-- 'password', 'createComputer_password' - A one-time password that is used to join the computer to the directory.
-- You should generate a random, strong password to use for this parameter.
newCreateComputer ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'computerName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateComputer
newCreateComputer
  pDirectoryId_
  pComputerName_
  pPassword_ =
    CreateComputer'
      { organizationalUnitDistinguishedName =
          Prelude.Nothing,
        computerAttributes = Prelude.Nothing,
        directoryId = pDirectoryId_,
        computerName = pComputerName_,
        password = Prelude._Sensitive Lens.# pPassword_
      }

-- | The fully-qualified distinguished name of the organizational unit to
-- place the computer account in.
createComputer_organizationalUnitDistinguishedName :: Lens.Lens' CreateComputer (Prelude.Maybe Prelude.Text)
createComputer_organizationalUnitDistinguishedName = Lens.lens (\CreateComputer' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@CreateComputer' {} a -> s {organizationalUnitDistinguishedName = a} :: CreateComputer)

-- | An array of Attribute objects that contain any LDAP attributes to apply
-- to the computer account.
createComputer_computerAttributes :: Lens.Lens' CreateComputer (Prelude.Maybe [Attribute])
createComputer_computerAttributes = Lens.lens (\CreateComputer' {computerAttributes} -> computerAttributes) (\s@CreateComputer' {} a -> s {computerAttributes = a} :: CreateComputer) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the directory in which to create the computer account.
createComputer_directoryId :: Lens.Lens' CreateComputer Prelude.Text
createComputer_directoryId = Lens.lens (\CreateComputer' {directoryId} -> directoryId) (\s@CreateComputer' {} a -> s {directoryId = a} :: CreateComputer)

-- | The name of the computer account.
createComputer_computerName :: Lens.Lens' CreateComputer Prelude.Text
createComputer_computerName = Lens.lens (\CreateComputer' {computerName} -> computerName) (\s@CreateComputer' {} a -> s {computerName = a} :: CreateComputer)

-- | A one-time password that is used to join the computer to the directory.
-- You should generate a random, strong password to use for this parameter.
createComputer_password :: Lens.Lens' CreateComputer Prelude.Text
createComputer_password = Lens.lens (\CreateComputer' {password} -> password) (\s@CreateComputer' {} a -> s {password = a} :: CreateComputer) Prelude.. Prelude._Sensitive

instance Prelude.AWSRequest CreateComputer where
  type Rs CreateComputer = CreateComputerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateComputerResponse'
            Prelude.<$> (x Prelude..?> "Computer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateComputer

instance Prelude.NFData CreateComputer

instance Prelude.ToHeaders CreateComputer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.CreateComputer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateComputer where
  toJSON CreateComputer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OrganizationalUnitDistinguishedName" Prelude..=)
              Prelude.<$> organizationalUnitDistinguishedName,
            ("ComputerAttributes" Prelude..=)
              Prelude.<$> computerAttributes,
            Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just
              ("ComputerName" Prelude..= computerName),
            Prelude.Just ("Password" Prelude..= password)
          ]
      )

instance Prelude.ToPath CreateComputer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateComputer where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results for the CreateComputer operation.
--
-- /See:/ 'newCreateComputerResponse' smart constructor.
data CreateComputerResponse = CreateComputerResponse'
  { -- | A Computer object that represents the computer account.
    computer :: Prelude.Maybe Computer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateComputerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computer', 'createComputerResponse_computer' - A Computer object that represents the computer account.
--
-- 'httpStatus', 'createComputerResponse_httpStatus' - The response's http status code.
newCreateComputerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateComputerResponse
newCreateComputerResponse pHttpStatus_ =
  CreateComputerResponse'
    { computer = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A Computer object that represents the computer account.
createComputerResponse_computer :: Lens.Lens' CreateComputerResponse (Prelude.Maybe Computer)
createComputerResponse_computer = Lens.lens (\CreateComputerResponse' {computer} -> computer) (\s@CreateComputerResponse' {} a -> s {computer = a} :: CreateComputerResponse)

-- | The response's http status code.
createComputerResponse_httpStatus :: Lens.Lens' CreateComputerResponse Prelude.Int
createComputerResponse_httpStatus = Lens.lens (\CreateComputerResponse' {httpStatus} -> httpStatus) (\s@CreateComputerResponse' {} a -> s {httpStatus = a} :: CreateComputerResponse)

instance Prelude.NFData CreateComputerResponse
