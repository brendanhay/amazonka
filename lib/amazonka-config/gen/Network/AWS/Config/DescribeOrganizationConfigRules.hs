{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeOrganizationConfigRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization config rules.
--
--
-- Only a master account and a delegated administrator account can call this API. When calling this API with a delegated administrator, you must ensure AWS Organizations @ListDelegatedAdministrator@ permissions are added. 
module Network.AWS.Config.DescribeOrganizationConfigRules
  ( -- * Creating a Request
    describeOrganizationConfigRules,
    DescribeOrganizationConfigRules,

    -- * Request Lenses
    docrOrganizationConfigRuleNames,
    docrNextToken,
    docrLimit,

    -- * Destructuring the Response
    describeOrganizationConfigRulesResponse,
    DescribeOrganizationConfigRulesResponse,

    -- * Response Lenses
    docrrsOrganizationConfigRules,
    docrrsNextToken,
    docrrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganizationConfigRules' smart constructor.
data DescribeOrganizationConfigRules = DescribeOrganizationConfigRules'
  { _docrOrganizationConfigRuleNames ::
      !(Maybe [Text]),
    _docrNextToken ::
      !(Maybe Text),
    _docrLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganizationConfigRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docrOrganizationConfigRuleNames' - The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
--
-- * 'docrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docrLimit' - The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
describeOrganizationConfigRules ::
  DescribeOrganizationConfigRules
describeOrganizationConfigRules =
  DescribeOrganizationConfigRules'
    { _docrOrganizationConfigRuleNames =
        Nothing,
      _docrNextToken = Nothing,
      _docrLimit = Nothing
    }

-- | The names of organization config rules for which you want details. If you do not specify any names, AWS Config returns details for all your organization config rules.
docrOrganizationConfigRuleNames :: Lens' DescribeOrganizationConfigRules [Text]
docrOrganizationConfigRuleNames = lens _docrOrganizationConfigRuleNames (\s a -> s {_docrOrganizationConfigRuleNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
docrNextToken :: Lens' DescribeOrganizationConfigRules (Maybe Text)
docrNextToken = lens _docrNextToken (\s a -> s {_docrNextToken = a})

-- | The maximum number of organization config rules returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
docrLimit :: Lens' DescribeOrganizationConfigRules (Maybe Natural)
docrLimit = lens _docrLimit (\s a -> s {_docrLimit = a}) . mapping _Nat

instance AWSRequest DescribeOrganizationConfigRules where
  type
    Rs DescribeOrganizationConfigRules =
      DescribeOrganizationConfigRulesResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRulesResponse'
            <$> (x .?> "OrganizationConfigRules" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOrganizationConfigRules

instance NFData DescribeOrganizationConfigRules

instance ToHeaders DescribeOrganizationConfigRules where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeOrganizationConfigRules" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeOrganizationConfigRules where
  toJSON DescribeOrganizationConfigRules' {..} =
    object
      ( catMaybes
          [ ("OrganizationConfigRuleNames" .=)
              <$> _docrOrganizationConfigRuleNames,
            ("NextToken" .=) <$> _docrNextToken,
            ("Limit" .=) <$> _docrLimit
          ]
      )

instance ToPath DescribeOrganizationConfigRules where
  toPath = const "/"

instance ToQuery DescribeOrganizationConfigRules where
  toQuery = const mempty

-- | /See:/ 'describeOrganizationConfigRulesResponse' smart constructor.
data DescribeOrganizationConfigRulesResponse = DescribeOrganizationConfigRulesResponse'
  { _docrrsOrganizationConfigRules ::
      !( Maybe
           [OrganizationConfigRule]
       ),
    _docrrsNextToken ::
      !( Maybe
           Text
       ),
    _docrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganizationConfigRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'docrrsOrganizationConfigRules' - Returns a list of @OrganizationConfigRule@ objects.
--
-- * 'docrrsNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'docrrsResponseStatus' - -- | The response status code.
describeOrganizationConfigRulesResponse ::
  -- | 'docrrsResponseStatus'
  Int ->
  DescribeOrganizationConfigRulesResponse
describeOrganizationConfigRulesResponse pResponseStatus_ =
  DescribeOrganizationConfigRulesResponse'
    { _docrrsOrganizationConfigRules =
        Nothing,
      _docrrsNextToken = Nothing,
      _docrrsResponseStatus = pResponseStatus_
    }

-- | Returns a list of @OrganizationConfigRule@ objects.
docrrsOrganizationConfigRules :: Lens' DescribeOrganizationConfigRulesResponse [OrganizationConfigRule]
docrrsOrganizationConfigRules = lens _docrrsOrganizationConfigRules (\s a -> s {_docrrsOrganizationConfigRules = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
docrrsNextToken :: Lens' DescribeOrganizationConfigRulesResponse (Maybe Text)
docrrsNextToken = lens _docrrsNextToken (\s a -> s {_docrrsNextToken = a})

-- | -- | The response status code.
docrrsResponseStatus :: Lens' DescribeOrganizationConfigRulesResponse Int
docrrsResponseStatus = lens _docrrsResponseStatus (\s a -> s {_docrrsResponseStatus = a})

instance NFData DescribeOrganizationConfigRulesResponse
