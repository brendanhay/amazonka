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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import qualified Network.AWS.Lens as Lens

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'newEnvironmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { -- | In a paginated request, the token that you can pass in a subsequent
    -- request to get the next response page.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns an EnvironmentDescription list.
    environments :: Core.Maybe [EnvironmentDescription]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnvironmentDescriptionsMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'environmentDescriptionsMessage_nextToken' - In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
--
-- 'environments', 'environmentDescriptionsMessage_environments' - Returns an EnvironmentDescription list.
newEnvironmentDescriptionsMessage ::
  EnvironmentDescriptionsMessage
newEnvironmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    { nextToken =
        Core.Nothing,
      environments = Core.Nothing
    }

-- | In a paginated request, the token that you can pass in a subsequent
-- request to get the next response page.
environmentDescriptionsMessage_nextToken :: Lens.Lens' EnvironmentDescriptionsMessage (Core.Maybe Core.Text)
environmentDescriptionsMessage_nextToken = Lens.lens (\EnvironmentDescriptionsMessage' {nextToken} -> nextToken) (\s@EnvironmentDescriptionsMessage' {} a -> s {nextToken = a} :: EnvironmentDescriptionsMessage)

-- | Returns an EnvironmentDescription list.
environmentDescriptionsMessage_environments :: Lens.Lens' EnvironmentDescriptionsMessage (Core.Maybe [EnvironmentDescription])
environmentDescriptionsMessage_environments = Lens.lens (\EnvironmentDescriptionsMessage' {environments} -> environments) (\s@EnvironmentDescriptionsMessage' {} a -> s {environments = a} :: EnvironmentDescriptionsMessage) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML EnvironmentDescriptionsMessage where
  parseXML x =
    EnvironmentDescriptionsMessage'
      Core.<$> (x Core..@? "NextToken")
      Core.<*> ( x Core..@? "Environments" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable EnvironmentDescriptionsMessage

instance Core.NFData EnvironmentDescriptionsMessage
