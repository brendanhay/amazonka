{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConfigRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified AWS Config rule and all of its evaluation results.
--
--
-- AWS Config sets the state of a rule to @DELETING@ until the deletion is complete. You cannot update a rule while it is in this state. If you make a @PutConfigRule@ or @DeleteConfigRule@ request for the rule, you will receive a @ResourceInUseException@ .
--
-- You can check the state of a rule by using the @DescribeConfigRules@ request.
--
module Network.AWS.Config.DeleteConfigRule
    (
    -- * Creating a Request
      deleteConfigRule
    , DeleteConfigRule
    -- * Request Lenses
    , dcrConfigRuleName

    -- * Destructuring the Response
    , deleteConfigRuleResponse
    , DeleteConfigRuleResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteConfigRule' smart constructor.
newtype DeleteConfigRule = DeleteConfigRule'
  { _dcrConfigRuleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrConfigRuleName' - The name of the AWS Config rule that you want to delete.
deleteConfigRule
    :: Text -- ^ 'dcrConfigRuleName'
    -> DeleteConfigRule
deleteConfigRule pConfigRuleName_ =
  DeleteConfigRule' {_dcrConfigRuleName = pConfigRuleName_}


-- | The name of the AWS Config rule that you want to delete.
dcrConfigRuleName :: Lens' DeleteConfigRule Text
dcrConfigRuleName = lens _dcrConfigRuleName (\ s a -> s{_dcrConfigRuleName = a})

instance AWSRequest DeleteConfigRule where
        type Rs DeleteConfigRule = DeleteConfigRuleResponse
        request = postJSON config
        response = receiveNull DeleteConfigRuleResponse'

instance Hashable DeleteConfigRule where

instance NFData DeleteConfigRule where

instance ToHeaders DeleteConfigRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteConfigRule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteConfigRule where
        toJSON DeleteConfigRule'{..}
          = object
              (catMaybes
                 [Just ("ConfigRuleName" .= _dcrConfigRuleName)])

instance ToPath DeleteConfigRule where
        toPath = const "/"

instance ToQuery DeleteConfigRule where
        toQuery = const mempty

-- | /See:/ 'deleteConfigRuleResponse' smart constructor.
data DeleteConfigRuleResponse =
  DeleteConfigRuleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConfigRuleResponse' with the minimum fields required to make a request.
--
deleteConfigRuleResponse
    :: DeleteConfigRuleResponse
deleteConfigRuleResponse = DeleteConfigRuleResponse'


instance NFData DeleteConfigRuleResponse where
