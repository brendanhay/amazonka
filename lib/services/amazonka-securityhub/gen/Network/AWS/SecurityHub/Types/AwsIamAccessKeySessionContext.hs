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
-- Module      : Network.AWS.SecurityHub.Types.AwsIamAccessKeySessionContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsIamAccessKeySessionContext where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsIamAccessKeySessionContextAttributes
import Network.AWS.SecurityHub.Types.AwsIamAccessKeySessionContextSessionIssuer

-- | Provides information about the session that the key was used for.
--
-- /See:/ 'newAwsIamAccessKeySessionContext' smart constructor.
data AwsIamAccessKeySessionContext = AwsIamAccessKeySessionContext'
  { -- | Attributes of the session that the key was used for.
    attributes :: Prelude.Maybe AwsIamAccessKeySessionContextAttributes,
    -- | Information about the entity that created the session.
    sessionIssuer :: Prelude.Maybe AwsIamAccessKeySessionContextSessionIssuer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamAccessKeySessionContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'awsIamAccessKeySessionContext_attributes' - Attributes of the session that the key was used for.
--
-- 'sessionIssuer', 'awsIamAccessKeySessionContext_sessionIssuer' - Information about the entity that created the session.
newAwsIamAccessKeySessionContext ::
  AwsIamAccessKeySessionContext
newAwsIamAccessKeySessionContext =
  AwsIamAccessKeySessionContext'
    { attributes =
        Prelude.Nothing,
      sessionIssuer = Prelude.Nothing
    }

-- | Attributes of the session that the key was used for.
awsIamAccessKeySessionContext_attributes :: Lens.Lens' AwsIamAccessKeySessionContext (Prelude.Maybe AwsIamAccessKeySessionContextAttributes)
awsIamAccessKeySessionContext_attributes = Lens.lens (\AwsIamAccessKeySessionContext' {attributes} -> attributes) (\s@AwsIamAccessKeySessionContext' {} a -> s {attributes = a} :: AwsIamAccessKeySessionContext)

-- | Information about the entity that created the session.
awsIamAccessKeySessionContext_sessionIssuer :: Lens.Lens' AwsIamAccessKeySessionContext (Prelude.Maybe AwsIamAccessKeySessionContextSessionIssuer)
awsIamAccessKeySessionContext_sessionIssuer = Lens.lens (\AwsIamAccessKeySessionContext' {sessionIssuer} -> sessionIssuer) (\s@AwsIamAccessKeySessionContext' {} a -> s {sessionIssuer = a} :: AwsIamAccessKeySessionContext)

instance Core.FromJSON AwsIamAccessKeySessionContext where
  parseJSON =
    Core.withObject
      "AwsIamAccessKeySessionContext"
      ( \x ->
          AwsIamAccessKeySessionContext'
            Prelude.<$> (x Core..:? "Attributes")
            Prelude.<*> (x Core..:? "SessionIssuer")
      )

instance
  Prelude.Hashable
    AwsIamAccessKeySessionContext

instance Prelude.NFData AwsIamAccessKeySessionContext

instance Core.ToJSON AwsIamAccessKeySessionContext where
  toJSON AwsIamAccessKeySessionContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Attributes" Core..=) Prelude.<$> attributes,
            ("SessionIssuer" Core..=) Prelude.<$> sessionIssuer
          ]
      )
